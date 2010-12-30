module Data.ASN1.Types
	( ASN1t(..)
	, ofStream
	, toStream
	) where

import qualified Data.ASN1.Stream as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import Data.Text.Lazy (Text)
import Data.ASN1.Raw (ASN1Class, ASN1Tag)

data ASN1t =
	  Boolean Bool
	| IntVal Integer
	| BitString Int L.ByteString
	| OctetString L.ByteString
	| Null
	| OID [Integer]
	| Real Double
	| Enumerated
	| UTF8String Text
	| NumericString L.ByteString
	| PrintableString Text
	| T61String L.ByteString
	| VideoTexString L.ByteString
	| IA5String Text
	| UTCTime (Int, Int, Int, Int, Int, Int, Bool)
	| GeneralizedTime (Int, Int, Int, Int, Int, Int, Bool)
	| GraphicString L.ByteString
	| VisibleString L.ByteString
	| GeneralString L.ByteString
	| UniversalString Text
	| CharacterString L.ByteString
	| BMPString Text
	| Other ASN1Class ASN1Tag ByteString
	| Sequence [ASN1t]
	| Set [ASN1t]
	| Container ASN1Class ASN1Tag [ASN1t]
	deriving (Show, Eq)

tillEnd :: Int -> [S.ASN1] -> ([S.ASN1],[S.ASN1])
tillEnd _ xs@[]                = (xs, xs)
tillEnd i ((x@(S.Start _)):xs) = let (yz, zs) = tillEnd (i+1) xs in (x:yz,zs)
tillEnd i ((x@(S.End _)):xs)
	| i == 0               = ([], xs)
	| otherwise            = let (ys, zs) = tillEnd (i-1) xs in (x:ys,zs)
tillEnd i (x:xs)               = let (ys, zs) = tillEnd i xs in (x:ys,zs)

ofStream :: [S.ASN1] -> [ASN1t]
ofStream []                        = []
ofStream (S.End _ : l)             = ofStream l

ofStream (S.Start ty : l)          =
	let (c, rest) = tillEnd 0 l in
	(case ty of
		S.Sequence         -> Sequence (ofStream c)
		S.Set              -> Set (ofStream c)
		S.Container tc tag -> Container tc tag (ofStream c)
	) : ofStream rest

ofStream (S.Boolean b : l)         = Boolean b : ofStream l
ofStream (S.IntVal i : l)          = IntVal i : ofStream l
ofStream (S.BitString i bits : l)  = BitString i bits : ofStream l
ofStream (S.OctetString b : l)     = OctetString b : ofStream l
ofStream (S.Null : l)              = Null : ofStream l
ofStream (S.OID oid : l)           = OID oid : ofStream l
ofStream (S.Real d : l)            = Real d : ofStream l
ofStream (S.Enumerated : l)        = Enumerated : ofStream l 
ofStream (S.UTF8String b : l)      = UTF8String b : ofStream l 
ofStream (S.NumericString b : l)   = NumericString b : ofStream l 
ofStream (S.PrintableString b : l) = PrintableString b : ofStream l 
ofStream (S.T61String b : l)       = T61String b : ofStream l 
ofStream (S.VideoTexString b : l)  = VideoTexString b : ofStream l 
ofStream (S.IA5String b : l)       = IA5String b : ofStream l 
ofStream (S.UTCTime t : l)         = UTCTime t : ofStream l 
ofStream (S.GeneralizedTime t : l) = GeneralizedTime t : ofStream l 
ofStream (S.GraphicString b : l)   = GraphicString b : ofStream l 
ofStream (S.VisibleString b : l)   = VisibleString b : ofStream l 
ofStream (S.GeneralString b : l)   = GeneralString b : ofStream l 
ofStream (S.UniversalString b : l) = UniversalString b : ofStream l 
ofStream (S.CharacterString b : l) = CharacterString b : ofStream l 
ofStream (S.BMPString b : l)       = BMPString b : ofStream l 
ofStream (S.Other c t b : l)       = Other c t b : ofStream l 

toStream :: [ASN1t] -> [S.ASN1]
toStream l = concatMap toStreamOne l
	where
		toStreamOne (Sequence s)          = ([S.Start S.Sequence] ++ toStream s ++ [S.End S.Sequence])
		toStreamOne (Set s)               = ([S.Start S.Set] ++ toStream s ++ [S.End S.Set])
		toStreamOne (Container tc tag s)  = ([S.Start (S.Container tc tag)] ++ toStream s ++ [S.End (S.Container tc tag)])
		toStreamOne (Boolean b)         = [S.Boolean b]
		toStreamOne (IntVal b)          = [S.IntVal b]
		toStreamOne (BitString i b)     = [S.BitString i b]
		toStreamOne (OctetString b)     = [S.OctetString b]
		toStreamOne (Null)              = [S.Null]
		toStreamOne (OID b)             = [S.OID b]
		toStreamOne (Real b)            = [S.Real b]
		toStreamOne (Enumerated)        = [S.Enumerated]
		toStreamOne (UTF8String b)      = [S.UTF8String b]
		toStreamOne (NumericString b)   = [S.NumericString b]
		toStreamOne (PrintableString b) = [S.PrintableString b]
		toStreamOne (T61String b)       = [S.T61String b]
		toStreamOne (VideoTexString b)  = [S.VideoTexString b]
		toStreamOne (IA5String b)       = [S.IA5String b]
		toStreamOne (UTCTime b)         = [S.UTCTime b]
		toStreamOne (GeneralizedTime b) = [S.GeneralizedTime b]
		toStreamOne (GraphicString b)   = [S.GraphicString b]
		toStreamOne (VisibleString b)   = [S.VisibleString b]
		toStreamOne (GeneralString b)   = [S.GeneralString b]
		toStreamOne (UniversalString b) = [S.UniversalString b]
		toStreamOne (CharacterString b) = [S.CharacterString b]
		toStreamOne (BMPString b)       = [S.BMPString b]
		toStreamOne (Other c t b)       = [S.Other c t b]
