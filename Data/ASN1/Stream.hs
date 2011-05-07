module Data.ASN1.Stream
	( ASN1(..)
	, ASN1Repr
	, ASN1ConstructionType(..)
	, getConstructedEnd
	, getConstructedEndRepr
	) where

import Data.ASN1.Event
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

data ASN1ConstructionType =
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
	| UTF8String String
	| NumericString L.ByteString
	| PrintableString String
	| T61String String
	| VideoTexString L.ByteString
	| IA5String String
	| UTCTime (Int, Int, Int, Int, Int, Int, Bool)
	| GeneralizedTime (Int, Int, Int, Int, Int, Int, Bool)
	| GraphicString L.ByteString
	| VisibleString L.ByteString
	| GeneralString L.ByteString
	| UniversalString String
	| CharacterString L.ByteString
	| BMPString String
	| Other ASN1Class ASN1Tag ByteString
	| Start ASN1ConstructionType
	| End ASN1ConstructionType
	| Event [ASN1Event]
	deriving (Show, Eq)

{- associate a list of asn1 event with an ASN1 type.
 - it's sometimes required to know the exact byte sequence leading to an ASN1 type:
 - eg: cryptographic signature -}
type ASN1Repr = (ASN1, [ASN1Event])

getConstructedEnd :: Int -> [ASN1] -> ([ASN1],[ASN1])
getConstructedEnd _ xs@[]                = (xs, [])
getConstructedEnd i ((x@(Start _)):xs)   = let (yz, zs) = getConstructedEnd (i+1) xs in (x:yz,zs)
getConstructedEnd i ((x@(End _)):xs)
	| i == 0    = ([], xs)
	| otherwise = let (ys, zs) = getConstructedEnd (i-1) xs in (x:ys,zs)
getConstructedEnd i (x:xs)               = let (ys, zs) = getConstructedEnd i xs in (x:ys,zs)

getConstructedEndRepr :: [ASN1Repr] -> ([ASN1Repr],[ASN1Repr])
getConstructedEndRepr = g
	where
		g []                 = ([], [])
		g (x@(Start _,_):xs) = let (ys, zs) = getEnd 1 xs in (x:ys, zs)
		g (x:xs)             = ([x],xs)

		getEnd :: Int -> [ASN1Repr] -> ([ASN1Repr],[ASN1Repr])
		getEnd _ []                    = ([], [])
		getEnd 0 xs                    = ([], xs)
		getEnd i ((x@(Start _, _)):xs) = let (ys, zs) = getEnd (i+1) xs in (x:ys,zs)
		getEnd i ((x@(End _, _)):xs)   = let (ys, zs) = getEnd (i-1) xs in (x:ys,zs)
		getEnd i (x:xs)                = let (ys, zs) = getEnd i xs in (x:ys,zs)
