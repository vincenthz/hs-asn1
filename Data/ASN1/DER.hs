-- |
-- Module      : Data.ASN1.DER
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing ASN1 DER specification serialization/derialization tools
--
module Data.ASN1.DER
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

	-- * DER serialize functions
	, decodeASN1
	, decodeASN1s
	, encodeASN1
	, encodeASN1s
	) where

import Data.ASN1.Raw (ASN1Class(..), ASN1Length(..), ASN1Header(..), ASN1Event(..), ASN1Err(..))
import qualified Data.ASN1.Raw as Raw

import Data.ASN1.Prim
import Data.ASN1.Types (ASN1t)

import qualified Data.ASN1.BER as BER

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

import Control.Monad.Trans (lift)
import Control.Exception

import Data.Enumerator (Iteratee, Enumeratee, ($$))
import Data.Enumerator.IO
import qualified Data.Enumerator as E

{- | Check if the length is the minimum possible and it's not indefinite -}
checkLength :: ASN1Length -> Maybe ASN1Err
checkLength LenIndefinite = Just $ ASN1PolicyFailed "DER" "indefinite length not allowed"
checkLength (LenShort _)  = Nothing
checkLength (LenLong n i)
	| n == 1 && i < 0x80  = Just $ ASN1PolicyFailed "DER" "long length should be a short length"
	| n == 1 && i >= 0x80 = Nothing
	| otherwise           = if i >= 2^((n-1)*8) && i < 2^(n*8)
		then Nothing
		else Just $ ASN1PolicyFailed "DER" "long length is not shortest"

checkRawDER :: Monad m => Enumeratee Raw.ASN1Event Raw.ASN1Event m a
checkRawDER (E.Continue k) = do
	x <- E.head
	case x of
		Nothing -> return $ E.Continue k
		Just l  ->
			let err = tyCheck l in
			if err == Nothing
				then do
					newStep <- lift $ E.runIteratee $ k $ E.Chunks [l]
					checkRawDER newStep
				else error "DER policy failed"
	where
		tyCheck (Header (ASN1Header _ _ _ len)) = checkLength len
		tyCheck _                               = Nothing

checkRawDER step = return step

{- | enumReadRaw is an enumeratee from raw events to asn1 -}
enumReadRaw :: Monad m => Enumeratee Raw.ASN1Event ASN1 m a
enumReadRaw = \f -> E.joinI (checkRawDER $$ BER.enumReadRaw f)

{- | enumWriteRaw is an enumeratee from asn1 to raw events -}
enumWriteRaw :: Monad m => Enumeratee ASN1 Raw.ASN1Event m a
enumWriteRaw = BER.enumWriteRaw

{-| enumReadBytes is an enumeratee converting from bytestring to ASN1
  it transforms chunks of bytestring into chunks of ASN1 objects -}
enumReadBytes :: Monad m => Enumeratee ByteString ASN1 m a
enumReadBytes = \f -> E.joinI (Raw.enumReadBytes $$ (BER.enumReadRaw f))

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

{-# DEPRECATED decodeASN1s "use stream types with decodeASN1Stream" #-}
decodeASN1s :: L.ByteString -> Either ASN1Err [ASN1t]
decodeASN1s = BER.decodeASN1s

{-# DEPRECATED decodeASN1 "use stream types with decodeASN1Stream" #-}
decodeASN1 :: L.ByteString -> Either ASN1Err ASN1t
decodeASN1 = BER.decodeASN1

{-# DEPRECATED encodeASN1s "use stream types with encodeASN1Stream" #-}
encodeASN1s :: [ASN1t] -> L.ByteString
encodeASN1s = BER.encodeASN1s

{-# DEPRECATED encodeASN1 "use stream types with encodeASN1Stream" #-}
encodeASN1 :: ASN1t -> L.ByteString
encodeASN1 = BER.encodeASN1
