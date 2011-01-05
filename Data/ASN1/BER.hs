-- |
-- Module      : Data.ASN1.BER
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing ASN1 BER specification serialization/derialization tools
--
module Data.ASN1.BER
	( ASN1Class(..)
	, ASN1(..)

	-- * enumeratee to transform between ASN1 and raw
	, enumReadRaw
	, enumWriteRaw

	-- * enumeratee to transform between ASN1 and bytes
	, enumReadBytes
	, enumWriteBytes

	-- * iterate over common representation to an ASN1 stream
	, iterateFile
	, iterateByteString

	-- * BER serialize functions
	, decodeASN1Stream
	, encodeASN1Stream

	-- * BER serialize functions, deprecated
	, decodeASN1
	, decodeASN1s
	, encodeASN1
	, encodeASN1s
	) where

import Data.ASN1.Raw (ASN1Header(..), ASN1Class(..), ASN1Err(..))
import qualified Data.ASN1.Raw as Raw

import Data.ASN1.Stream
import Data.ASN1.Types (ofStream, toStream, ASN1t)
import Data.ASN1.Prim

import Control.Monad.Identity
import Control.Exception

import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)

import Data.Enumerator.IO
import Data.Enumerator (Iteratee(..), Enumeratee, ($$))
import qualified Data.Enumerator as E

decodeConstruction :: ASN1Header -> ConstructionType
decodeConstruction (ASN1Header Universal 0x10 _ _) = Sequence
decodeConstruction (ASN1Header Universal 0x11 _ _) = Set
decodeConstruction (ASN1Header c t _ _)            = Container c t

{- | enumReadRaw is an enumeratee from raw events to asn1 -}
enumReadRaw :: Monad m => Enumeratee Raw.ASN1Event ASN1 m a
enumReadRaw = step [] where
	step l (E.Continue k) = do
		x <- E.head
		case x of
			Nothing -> return $ E.Continue k
			Just Raw.ConstructionEnd -> do
				newStep <- lift $ E.runIteratee $ k $ E.Chunks [head l]
				step (tail l) newStep
			Just (Raw.Header hdr@(ASN1Header _ _ True _)) -> do
				z <- E.head
				let ctype = decodeConstruction hdr
				newStep <- case z of
					Nothing                    -> error "partial construction got EOF"
					Just Raw.ConstructionBegin -> do
						lift $ runIteratee $ k $ E.Chunks [Start ctype]
					Just _                     -> error "expecting construction"
				step (End ctype : l) newStep
			Just (Raw.Header hdr@(ASN1Header _ _ False _)) -> do
				z <- E.head
				newStep <- case z of
					Nothing -> error "header without a primitive"
					Just (Raw.Primitive p) -> do
						let (Right pr) = decodePrimitive hdr p
						lift $ runIteratee $ k $ E.Chunks [pr]
					Just _ -> error "expecting primitive"
				step l newStep
			Just _ -> do
				newStep <- lift $ runIteratee $ k $ E.Chunks []
				step l newStep
	step _ x = return x

{- | enumWriteRaw is an enumeratee from asn1 to raw events -}
enumWriteRaw :: Monad m => Enumeratee ASN1 Raw.ASN1Event m a
enumWriteRaw = \f -> E.joinI (enumWriteTree $$ (enumWriteTreeRaw f))

enumWriteTree :: Monad m => Enumeratee ASN1 (ASN1, [ASN1]) m a
enumWriteTree (E.Continue k) = do
	x <- E.head
	case x of
		Nothing         -> return $ E.Continue k
		Just n@(Start ty) -> do
			y <- consumeTillEnd
			newStep <- lift $ E.runIteratee $ k $ E.Chunks [ (n, y) ]
			enumWriteTree newStep
		Just p          -> do
			newStep <- lift $ E.runIteratee $ k $ E.Chunks [ (p, []) ]
			enumWriteTree newStep
	where
		consumeTillEnd :: Monad m => Iteratee ASN1 m [ASN1]
		consumeTillEnd = E.liftI $ step (1 :: Int) id where
			step l acc chunk = case chunk of
				E.Chunks [] -> E.Continue $ E.returnI . step l acc
				E.Chunks xs -> do
					let (ys, zs) = spanEnd l xs
					let nbend = length $ filter isEnd ys
					let nbstart = length $ filter isStart ys
					let nl = l - nbend + nbstart
					if nl == 0
						then E.Yield (acc ys) (E.Chunks zs)
						else E.Continue $ E.returnI . (step nl $ acc . (ys ++))
				E.EOF       -> E.Yield (acc []) E.EOF

			spanEnd :: Int -> [ASN1] -> ([ASN1], [ASN1])
			spanEnd _ []               = ([], [])
			spanEnd 0 (x@(End _):xs)   = ([x], xs)
			spanEnd 0 (x@(Start _):xs) = let (ys, zs) = spanEnd 1 xs in (x:ys, zs)
			spanEnd 0 (x:xs)           = let (ys, zs) = spanEnd 0 xs in (x:ys, zs)
			spanEnd l (x:xs)           = case x of
				Start _ -> let (ys, zs) = spanEnd (l+1) xs in (x:ys, zs)
				End _   -> let (ys, zs) = spanEnd (l-1) xs in (x:ys, zs)
				_       -> let (ys, zs) = spanEnd l xs in (x:ys, zs)

			isStart (Start _) = True
			isStart _         = False
			isEnd (End _)     = True
			isEnd _           = False

enumWriteTree step = return step

enumWriteTreeRaw :: Monad m => Enumeratee (ASN1, [ASN1]) Raw.ASN1Event m a
enumWriteTreeRaw = E.concatMap writeTree
	where writeTree (p,children) = snd $ case p of
		Start _ -> encodeConstructed p children
		_       -> encodePrimitive p

{-| enumReadBytes is an enumeratee converting from bytestring to ASN1
  it transforms chunks of bytestring into chunks of ASN1 objects -}
enumReadBytes :: Monad m => Enumeratee ByteString ASN1 m a
enumReadBytes = \f -> E.joinI (Raw.enumReadBytes $$ (enumReadRaw f))

{-| enumWriteBytes is an enumeratee converting from ASN1 to bytestring.
  it transforms chunks of ASN1 objects into chunks of bytestring  -}
enumWriteBytes :: Monad m => Enumeratee ASN1 ByteString m a
enumWriteBytes = \f -> E.joinI (enumWriteRaw $$ (Raw.enumWriteBytes f))

{-| iterate over a file using a file enumerator. -}
iterateFile :: FilePath -> Iteratee ASN1 IO a -> IO (Either SomeException a)
iterateFile path p = E.run (enumFile path $$ E.joinI $ enumReadBytes $$ p)

{-| iterate over a bytestring using a list enumerator over each chunks -}
iterateByteString :: Monad m => L.ByteString -> Iteratee ASN1 m a -> m (Either SomeException a)
iterateByteString bs p = E.run (E.enumList 1 (L.toChunks bs) $$ E.joinI $ enumReadBytes $$ p)

{-| decode a lazy bytestring as an ASN1 stream -}
decodeASN1Stream :: L.ByteString -> Either ASN1Err [ASN1]
decodeASN1Stream l = do
	case runIdentity (iterateByteString l E.consume) of
		Left err -> Left (maybe (ASN1ParsingFail "unknown") id $ fromException err)
		Right x  -> Right x

encodeASN1Stream :: Monad m => [ASN1] -> Iteratee ByteString m a -> m (Either SomeException a)
encodeASN1Stream l p = E.run (E.enumList 1 l $$ E.joinI $ enumWriteBytes $$ p)

{-# DEPRECATED decodeASN1s "use stream types with decodeASN1Stream" #-}
decodeASN1s :: L.ByteString -> Either ASN1Err [ASN1t]
decodeASN1s l = either (Left) (Right . ofStream) $ decodeASN1Stream l

{-# DEPRECATED decodeASN1 "use stream types with decodeASN1Stream" #-}
decodeASN1 :: L.ByteString -> Either ASN1Err ASN1t
decodeASN1 = either (Left) (Right . head) . decodeASN1s

{-# DEPRECATED encodeASN1s "use stream types with encodeASN1Stream" #-}
encodeASN1s :: [ASN1t] -> L.ByteString
encodeASN1s l = case runIdentity (encodeASN1Stream (toStream l) E.consume) of
	Left _  -> error "encoding failed"
	Right x -> L.fromChunks x

{-# DEPRECATED encodeASN1 "use stream types with encodeASN1Stream" #-}
encodeASN1 :: ASN1t -> L.ByteString
encodeASN1 = encodeASN1s . (:[])
