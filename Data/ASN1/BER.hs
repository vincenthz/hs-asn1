-- |
-- Module      : Data.ASN1.BER
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing ASN1 BER specification serialization/derialization tools
--
module Data.ASN1.BER (
	TagClass(..),
	ASN1(..),

	-- * BER interface when using directly Raw objects
	ofRaw,
	toRaw,

	-- * BER serial functions
	decodeASN1Get,
	decodeASN1,
	encodeASN1Put,
	encodeASN1
	) where

import Data.ASN1.Raw
import Data.ASN1.Prim
import Data.Either
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

ofRaws :: [Value] -> Either ASN1Err [ASN1]
ofRaws x = if l == [] then Right r else Left $ ASN1Multiple l
	where
		(l, r) = partitionEithers $ map ofRaw x

ofRaw :: Value -> Either ASN1Err ASN1
ofRaw (Value Universal 0x0 (Primitive b))    = getEOC b
ofRaw (Value Universal 0x1 (Primitive b))    = getBoolean False b
ofRaw (Value Universal 0x2 (Primitive b))    = getInteger b
ofRaw (Value Universal 0x3 v)                = getBitString v
ofRaw (Value Universal 0x4 v)                = getOctetString v
ofRaw (Value Universal 0x5 (Primitive b))    = getNull b
ofRaw (Value Universal 0x6 (Primitive b))    = getOID b
ofRaw (Value Universal 0x7 (Primitive _))    = Left $ ASN1NotImplemented "Object Descriptor"
ofRaw (Value Universal 0x8 (Constructed _))  = Left $ ASN1NotImplemented "External"
ofRaw (Value Universal 0x9 (Primitive _))    = Left $ ASN1NotImplemented "real"
ofRaw (Value Universal 0xa (Primitive _))    = Left $ ASN1NotImplemented "enumerated"
ofRaw (Value Universal 0xb (Constructed _))  = Left $ ASN1NotImplemented "EMBEDDED PDV"
ofRaw (Value Universal 0xc v)                = getUTF8String v
ofRaw (Value Universal 0xd (Primitive _))    = Left $ ASN1NotImplemented "RELATIVE-OID"
ofRaw (Value Universal 0x10 (Constructed l)) = either Left (Right . Sequence) $ ofRaws l
ofRaw (Value Universal 0x11 (Constructed l)) = either Left (Right . Set) $ ofRaws l
ofRaw (Value Universal 0x12 v)               = getNumericString v
ofRaw (Value Universal 0x13 v)               = getPrintableString v
ofRaw (Value Universal 0x14 v)               = getT61String v
ofRaw (Value Universal 0x15 v)               = getVideoTexString v
ofRaw (Value Universal 0x16 v)               = getIA5String v
ofRaw (Value Universal 0x17 x)               = getUTCTime x
ofRaw (Value Universal 0x18 x)               = getGeneralizedTime x
ofRaw (Value Universal 0x19 _)               = Left $ ASN1NotImplemented "x"
ofRaw (Value Universal 0x1a _)               = Left $ ASN1NotImplemented "x"
ofRaw (Value Universal 0x1b _)               = Left $ ASN1NotImplemented "x"
ofRaw (Value Universal 0x1c _)               = Left $ ASN1NotImplemented "x"
ofRaw (Value Universal 0x1d _)               = Left $ ASN1NotImplemented "CHARACTER STRING"
ofRaw (Value Universal 0x1e _)               = Left $ ASN1NotImplemented "BMPString"
ofRaw (Value tc tn (Primitive b))            = Right $ Other tc tn (Left b)
ofRaw (Value tc tn (Constructed l))          = either Left (Right . Other tc tn . Right) $ ofRaws l

toRaw :: ASN1 -> Value
toRaw EOC                    = Value Universal 0x0 (Primitive B.empty)
toRaw (Boolean v)            = Value Universal 0x1 (Primitive $ B.singleton (if v then 0xff else 0))
toRaw (IntVal i)             = Value Universal 0x2 (putInteger i)
toRaw (BitString i bits)     = Value Universal 0x3 (putBitString i bits)
toRaw (OctetString b)        = Value Universal 0x4 (putString b)
toRaw Null                   = Value Universal 0x5 (Primitive B.empty)
toRaw (OID oid)              = Value Universal 0x6 (putOID oid)
toRaw (Real f)               = Value Universal 0x9 (Constructed []) -- not implemented
toRaw Enumerated             = Value Universal 0xa (Constructed []) -- not implemented
toRaw (UTF8String b)         = Value Universal 0xc (putString b)
toRaw (Sequence children)    = Value Universal 0x10 (Constructed $ map toRaw children)
toRaw (Set children)         = Value Universal 0x11 (Constructed $ map toRaw children)
toRaw (NumericString b)      = Value Universal 0x12 (putString b)
toRaw (PrintableString b)    = Value Universal 0x13 (putString b)
toRaw (T61String b)          = Value Universal 0x14 (putString b)
toRaw (VideoTexString b)     = Value Universal 0x15 (putString b)
toRaw (IA5String b)          = Value Universal 0x16 (putString b)
toRaw (UTCTime time)         = Value Universal 0x17 (putUTCTime time)
toRaw (GeneralizedTime time) = Value Universal 0x18 (putGeneralizedTime time)
toRaw (GraphicString b)      = Value Universal 0x19 (putString b)
toRaw (VisibleString b)      = Value Universal 0x1a (putString b)
toRaw (GeneralString b)      = Value Universal 0x1b (putString b)
toRaw (UniversalString b)    = Value Universal 0x1c (putString b)
toRaw (Other tc tn c)        = Value tc tn (either Primitive (Constructed . map toRaw) c)

decodeASN1Get :: Get (Either ASN1Err ASN1)
decodeASN1Get = either Left ofRaw `fmap` runGetErrInGet getValue

decodeASN1 :: L.ByteString -> Either ASN1Err ASN1
decodeASN1 = either Left ofRaw . runGetErr getValue

encodeASN1Put :: ASN1 -> Put
encodeASN1Put = putValue . toRaw

encodeASN1 :: ASN1 -> L.ByteString
encodeASN1 = runPut . encodeASN1Put
