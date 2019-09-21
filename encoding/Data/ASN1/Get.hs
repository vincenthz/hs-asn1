-- |
-- Module      : Data.ASN1.Get
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Simple get module with really simple accessor for ASN1.
--
-- Original code is pulled from the Get module from cereal
-- which is covered by:
-- Copyright   : Lennart Kolmodin, Galois Inc. 2009
-- License     : BSD3-style (see LICENSE)
--
-- The original code has been tailored and reduced to only cover the useful
-- case for asn1 and augmented by a position.
--
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
module Data.ASN1.Get
    ( Result(..)
    , Input
    , Get
    , runGetPos
    , runGet
    , getBytes
    , getBytesCopy
    , getWord8
    ) where

import Control.Applicative (Applicative(..),Alternative(..))
import Control.Monad (ap,MonadPlus(..))
import Data.Maybe (fromMaybe)
import Foreign

import qualified Data.ByteString          as B

-- | The result of a parse.
data Result r = Fail String
              -- ^ The parse failed. The 'String' is the
              --   message describing the error, if any.
              | Partial (B.ByteString -> Result r)
              -- ^ Supply this continuation with more input so that
              --   the parser can resume. To indicate that no more
              --   input is available, use an 'B.empty' string.
              | Done r Position B.ByteString
              -- ^ The parse succeeded.  The 'B.ByteString' is the
              --   input that had not yet been consumed (if any) when
              --   the parse succeeded.

instance Show r => Show (Result r) where
    show (Fail msg)  = "Fail " ++ show msg
    show (Partial _) = "Partial _"
    show (Done r pos bs) = "Done " ++ show r ++ " " ++ show pos ++ " " ++ show bs

instance Functor Result where
    fmap _ (Fail msg)  = Fail msg
    fmap f (Partial k) = Partial (fmap f . k)
    fmap f (Done r p bs) = Done (f r) p bs

type Input  = B.ByteString
type Buffer = Maybe B.ByteString

type Failure   r = Input -> Buffer -> More -> Position -> String -> Result r
type Success a r = Input -> Buffer -> More -> Position -> a      -> Result r
type Position    = Word64

-- | Have we read all available input?
data More = Complete
          | Incomplete (Maybe Int)
          deriving (Eq)

-- | The Get monad is an Exception and State monad.
newtype Get a = Get
    { unGet :: forall r. Input -> Buffer -> More -> Position -> Failure r -> Success a r -> Result r }

append :: Buffer -> Buffer -> Buffer
append l r = B.append `fmap` l <*> r
{-# INLINE append #-}

bufferBytes :: Buffer -> B.ByteString
bufferBytes  = fromMaybe B.empty
{-# INLINE bufferBytes #-}

instance Functor Get where
    fmap p m =
      Get $ \s0 b0 m0 p0 kf ks ->
        let ks' s1 b1 m1 p1 a = ks s1 b1 m1 p1 (p a)
         in unGet m s0 b0 m0 p0 kf ks'

instance Applicative Get where
    pure  = return
    (<*>) = ap

instance Alternative Get where
    empty = failDesc "empty"
    (<|>) = mplus

-- Definition directly from Control.Monad.State.Strict
instance Monad Get where
    return a = Get $ \ s0 b0 m0 p0 _ ks -> ks s0 b0 m0 p0 a

    m >>= g  = Get $ \s0 b0 m0 p0 kf ks ->
        let ks' s1 b1 m1 p1 a = unGet (g a) s1 b1 m1 p1 kf ks
         in unGet m s0 b0 m0 p0 kf ks'

#if MIN_VERSION_base(4,13,0)
instance MonadFail Get where
#endif
    fail = failDesc

instance MonadPlus Get where
    mzero     = failDesc "mzero"
    mplus a b =
      Get $ \s0 b0 m0 p0 kf ks ->
        let kf' _ b1 m1 p1 _ = unGet b (s0 `B.append` bufferBytes b1)
                                       (b0 `append` b1) m1 p1 kf ks
         in unGet a s0 (Just B.empty) m0 p0 kf' ks

------------------------------------------------------------------------

put :: Position -> B.ByteString -> Get ()
put pos s = Get (\_ b0 m p0 _ k -> k s b0 m (p0+pos) ())
{-# INLINE put #-}

finalK :: B.ByteString -> t -> t1 -> Position -> r -> Result r
finalK s _ _ p a = Done a p s

failK :: Failure a
failK _ _ _ p s = Fail (show p ++ ":" ++ s)

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGetPos :: Position -> Get a -> B.ByteString -> Result a
runGetPos pos m str = unGet m str Nothing (Incomplete Nothing) pos failK finalK
{-# INLINE runGetPos #-}

runGet :: Get a -> B.ByteString -> Result a
runGet = runGetPos 0
{-# INLINE runGet #-}

-- | If at least @n@ bytes of input are available, return the current
--   input, otherwise fail.
ensure :: Int -> Get B.ByteString
ensure n = n `seq` Get $ \ s0 b0 m0 p0 kf ks ->
    if B.length s0 >= n
    then ks s0 b0 m0 p0 s0
    else unGet (demandInput >> ensureRec n) s0 b0 m0 p0 kf ks
{-# INLINE ensure #-}

-- | If at least @n@ bytes of input are available, return the current
--   input, otherwise fail.
ensureRec :: Int -> Get B.ByteString
ensureRec n = Get $ \s0 b0 m0 p0 kf ks ->
    if B.length s0 >= n
    then ks s0 b0 m0 p0 s0
    else unGet (demandInput >> ensureRec n) s0 b0 m0 p0 kf ks

-- | Immediately demand more input via a 'Partial' continuation
--   result.
demandInput :: Get ()
demandInput = Get $ \s0 b0 m0 p0 kf ks ->
  case m0 of
    Complete      -> kf s0 b0 m0 p0 "too few bytes"
    Incomplete mb -> Partial $ \s ->
      if B.null s
      then kf s0 b0 m0 p0 "too few bytes"
      else let update l = l - B.length s
               s1 = s0 `B.append` s
               b1 = b0 `append` Just s
            in ks s1 b1 (Incomplete (update `fmap` mb)) p0 ()

failDesc :: String -> Get a
failDesc err = Get (\s0 b0 m0 p0 kf _ -> kf s0 b0 m0 p0 ("Failed reading: " ++ err))

------------------------------------------------------------------------
-- Utility with ByteStrings

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input. This function creates a fresh
-- copy of the underlying bytes.
getBytesCopy :: Int -> Get B.ByteString
getBytesCopy n = do
  bs <- getBytes n
  return $! B.copy bs

------------------------------------------------------------------------
-- Helpers

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get B.ByteString
getBytes n
  | n <= 0    = return B.empty
  | otherwise = do
    s <- ensure n
    let (b1, b2) = B.splitAt n s
    put (fromIntegral n) b2
    return b1

getWord8 :: Get Word8
getWord8 = do
    s <- ensure 1
    case B.uncons s of
        Nothing     -> error "getWord8: ensure internal error"
        Just (h,b2) -> put 1 b2 >> return h
