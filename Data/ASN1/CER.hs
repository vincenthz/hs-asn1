-- |
-- Module      : Data.ASN1.CER
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing ASN1 CER specification serialization/derialization tools
--
module Data.ASN1.CER
	( ASN1Class(..)
	, ASN1(..)

	-- * CER serial functions
	, decodeASN1Get
	, decodeASN1
	, encodeASN1Put
	, encodeASN1
	) where

import Data.ASN1.Raw
import Data.ASN1.Prim
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ASN1.BER as BER
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy.Encoding (encodeUtf8, encodeUtf32BE)

{- | check if the value is bounded by CER policies -}
check :: (ASN1Class, Bool, ASN1Tag) -> ASN1Length -> Maybe ASN1Err
check (_, _, _) _ = Nothing

{- | ofRaw same as BER.ofRAW but check some additional CER constraint. -}
ofRaw :: Value -> Either ASN1Err ASN1
ofRaw v = BER.ofRaw v


{-
- 9.2        String encoding forms
string values shall be encoded with a primitive encoding if they would
require no more than 1000 contents octets, and as a constructed encoding otherwise.
The string fragments contained in the constructed encoding shall be encoded with a primitive encoding.
The encoding of each fragment, except possibly the last, shall have 1000 contents octets.
-}
putStringCER :: Int -> L.ByteString -> ValStruct
putStringCER tn l =
	if L.length l > 1000
		then Constructed $ map (Value Universal tn . (Primitive . B.concat . L.toChunks)) $ repack1000 l
		else Primitive $ B.concat $ L.toChunks l
	where
		repack1000 x =
			if L.length x > 1000
				then 
					let (x1, x2) = L.splitAt 1000 x in
					x1 : repack1000 x2
				else
					[ x ]

{- | toRaw create a CER encoded value ready -}
toRaw :: ASN1 -> Value
toRaw (BitString i bits)     = Value Universal 0x3 (putBitString i bits)
toRaw (OctetString b)        = Value Universal 0x4 (putStringCER 0x4 b)
toRaw (UTF8String b)         = Value Universal 0xc (putStringCER 0xc (encodeUtf8 b))
toRaw (NumericString b)      = Value Universal 0x12 (putStringCER 0x12 b)
toRaw (PrintableString b)    = Value Universal 0x13 (putStringCER 0x13 (encodeUtf8 b))
toRaw (T61String b)          = Value Universal 0x14 (putStringCER 0x14 b)
toRaw (VideoTexString b)     = Value Universal 0x15 (putStringCER 0x15 b)
toRaw (IA5String b)          = Value Universal 0x16 (putStringCER 0x16 (encodeUtf8 b))
toRaw (GraphicString b)      = Value Universal 0x19 (putStringCER 0x19 b)
toRaw (VisibleString b)      = Value Universal 0x1a (putStringCER 0x1a b)
toRaw (GeneralString b)      = Value Universal 0x1b (putStringCER 0x1b b)
toRaw (UniversalString b)    = Value Universal 0x1c (putStringCER 0x1c (encodeUtf32BE b))
toRaw (BMPString b)          = Value Universal 0x1e (putStringCER 0x1e (encodeUCS2BE b))
toRaw v                      = BER.toRaw v

decodeASN1Get :: Get (Either ASN1Err ASN1)
decodeASN1Get = runGetErrInGet (getValueCheck check) >>= return . either Left ofRaw

decodeASN1 :: L.ByteString -> Either ASN1Err ASN1
decodeASN1 b = either Left ofRaw $ runGetErr (getValueCheck check) b

encodePolicyCER :: Value -> Int -> ASN1Length
encodePolicyCER (Value _ _ (Primitive _)) len
	| len < 0x80   = LenShort len
	| otherwise    = LenLong 0 len
encodePolicyCER (Value _ _ (Constructed _)) _ = LenIndefinite

encodeASN1Put :: ASN1 -> Put
encodeASN1Put d = putValuePolicy encodePolicyCER $ toRaw d

encodeASN1 :: ASN1 -> L.ByteString
encodeASN1 = runPut . encodeASN1Put
