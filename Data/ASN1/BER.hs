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

	, parseEvents
	, writeEvents
	, iterateFile
	, iterateByteString
	-- * BER serial functions
	, decodeASN1Stream
	, encodeASN1Stream
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

import Control.Monad.Error
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

parseEvents :: Monad m => [ASN1] -> Enumeratee Raw.ASN1Event ASN1 m a
parseEvents l (E.Continue k) = do
	x <- E.head
	case x of
		Nothing -> return $ E.Continue k
		Just Raw.ConstructionEnd -> do
			newStep <- lift $ E.runIteratee $ k $ E.Chunks [head l]
			parseEvents (tail l) newStep
		Just (Raw.Header hdr@(ASN1Header _ _ True _)) -> do
			z <- E.head
			let ctype = decodeConstruction hdr
			newStep <- case z of
				Nothing                    -> error "partial construction got EOF"
				Just Raw.ConstructionBegin -> do
					lift $ runIteratee $ k $ E.Chunks [Start ctype]
				Just _                     -> error "expecting construction"
			parseEvents (End ctype : l) newStep
		Just (Raw.Header hdr@(ASN1Header _ _ False _)) -> do
			z <- E.head
			newStep <- case z of
				Nothing -> error "header without a primitive"
				Just (Raw.Primitive p) -> do
					let (Right pr) = decodePrimitive hdr p
					lift $ runIteratee $ k $ E.Chunks [pr]
				Just _ -> error "expecting primitive"
			parseEvents l newStep
		Just _ -> do
			newStep <- lift $ runIteratee $ k $ E.Chunks []
			parseEvents l newStep

parseEvents _ step = return step

writeEvents :: Monad m => Enumeratee ASN1 Raw.ASN1Event m a
writeEvents (E.Continue k) = do
	x <- E.head
	case x of
		Nothing         -> return $ E.Continue k
		Just (Start ty) -> do
			newStep <- lift $ E.runIteratee $ k $ E.Chunks []
			writeEvents newStep
		Just p          -> do
			let (h, b) = encodePrimitive p
			newStep <- lift $ E.runIteratee $ k $ E.Chunks [ Raw.Header h, Raw.Primitive b ]
			writeEvents newStep

writeEvents step           = return step

{-| iterate over a file using a file enumerator. -}
iterateFile :: FilePath -> Iteratee ASN1 IO a -> IO (Either SomeException a)
iterateFile path p = E.run (enumFile path $$ E.joinI $ Raw.parseBytes $$ E.joinI $ parseEvents [] $$ p)

{-| iterate over a bytestring using a list enumerator over each chunks -}
iterateByteString :: Monad m => L.ByteString -> Iteratee ASN1 m a -> m (Either SomeException a)
iterateByteString bs p = E.run (E.enumList 1 (L.toChunks bs) $$ E.joinI $ Raw.parseBytes $$ E.joinI $ parseEvents [] $$ p) 

{-| decode a lazy bytestring as an ASN1 stream -}
decodeASN1Stream :: L.ByteString -> Either ASN1Err [ASN1]
decodeASN1Stream l = do
	r <- iterateByteString l E.consume
	case r of
		Left _  -> Left ASN1ParsingFail
		Right x -> Right x

encodeASN1Stream :: Monad m => [ASN1] -> Iteratee ByteString m a -> m (Either SomeException a)
encodeASN1Stream l p = E.run (E.enumList 1 l $$ E.joinI $ writeEvents $$ E.joinI $ Raw.writeBytes $$ p)

{-# DEPRECATED decodeASN1s "use stream types with decodeASN1Stream" #-}
decodeASN1s :: L.ByteString -> Either ASN1Err [ASN1t]
decodeASN1s l = either (Left) (Right . ofStream) $ decodeASN1Stream l

{-# DEPRECATED decodeASN1 "use stream types with decodeASN1Stream" #-}
decodeASN1 :: L.ByteString -> Either ASN1Err ASN1t
decodeASN1 = either (Left) (Right . head) . decodeASN1s

{-# DEPRECATED encodeASN1s "use stream types with encodeASN1Stream" #-}
encodeASN1s :: [ASN1t] -> L.ByteString
encodeASN1s l = case runIdentity (encodeASN1Stream (toStream l) E.consume) of
	Left err -> error "encoding failed"
	Right x  -> L.fromChunks x

{-# DEPRECATED encodeASN1 "use stream types with encodeASN1Stream" #-}
encodeASN1 :: ASN1t -> L.ByteString
encodeASN1 = encodeASN1s . (:[])
