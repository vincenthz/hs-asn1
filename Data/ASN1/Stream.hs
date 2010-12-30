module Data.ASN1.Stream
	( ASN1(..)
	, ConstructionType(..)
	) where

import Data.ASN1.Raw
import Data.Text.Lazy (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

data ConstructionType =
	  Sequence
	| Set
	| Container ASN1Class ASN1Tag
	deriving (Show,Eq)

data ASN1 =
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
	| Start ConstructionType
	| End ConstructionType
	deriving (Show, Eq)
