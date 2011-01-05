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
	, ASN1ConstructionType(..)

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
import Data.Enumerator (Iteratee(..), Enumeratee, ($$), (>>==))
import qualified Data.Enumerator as E

decodeConstruction :: ASN1Header -> ASN1ConstructionType
decodeConstruction (ASN1Header Universal 0x10 _ _) = Sequence
decodeConstruction (ASN1Header Universal 0x11 _ _) = Set
decodeConstruction (ASN1Header c t _ _)            = Container c t

{- | enumReadRaw is an enumeratee from raw events to asn1 -}
enumReadRaw :: Monad m => Enumeratee Raw.ASN1Event ASN1 m a
enumReadRaw = E.checkDone $ \k -> k (E.Chunks []) >>== loop []
	where
		loop l = E.checkDone $ go l
		go l k = E.head >>= \x -> case x of
			Nothing                  ->
				if l == [] then  k (E.Chunks []) >>== return else E.throwError (Raw.ASN1ParsingPartial)
			Just Raw.ConstructionEnd ->
				k (E.Chunks [head l]) >>== loop (tail l)
			Just (Raw.Header hdr@(ASN1Header _ _ True _)) -> E.head >>= \z -> case z of
				Nothing                    -> E.throwError (Raw.ASN1ParsingFail "expecting construction, got EOF")
				Just Raw.ConstructionBegin ->
					let ctype = decodeConstruction hdr in
					k (E.Chunks [Start ctype]) >>== loop (End ctype : l)
				Just _                     -> E.throwError (Raw.ASN1ParsingFail "expecting construction")
			Just (Raw.Header hdr@(ASN1Header _ _ False _)) -> E.head >>= \z -> case z of
				Nothing -> E.throwError (Raw.ASN1ParsingFail "expecting primitive, got EOF")
				Just (Raw.Primitive p) ->
					let (Right pr) = decodePrimitive hdr p in
					k (E.Chunks [pr]) >>== loop l
				Just _  -> E.throwError (Raw.ASN1ParsingFail "expecting primitive")
			Just _ -> E.throwError (Raw.ASN1ParsingFail "boundary not a header")

{- | enumWriteRaw is an enumeratee from asn1 to raw events -}
enumWriteRaw :: Monad m => Enumeratee ASN1 Raw.ASN1Event m a
enumWriteRaw = \f -> E.joinI (enumWriteTree $$ (enumWriteTreeRaw f))

enumWriteTree :: Monad m => Enumeratee ASN1 (ASN1, [ASN1]) m a
enumWriteTree = do
	E.checkDone $ \k -> k (E.Chunks []) >>== loop
	where
		loop = E.checkDone $ go
		go k = E.head >>= \x -> case x of
			Nothing          -> k (E.Chunks []) >>== return
			Just n@(Start _) -> consumeTillEnd >>= \y -> k (E.Chunks [(n, y)] ) >>== loop
			Just p           -> k (E.Chunks [(p, [])] ) >>== loop

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
