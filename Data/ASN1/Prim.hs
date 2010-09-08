-- |
-- Module      : Data.ASN1.Prim
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Tools to read ASN1 primitive (e.g. boolean, int)
--

module Data.ASN1.Prim (
	-- * ASN1 high level algebraic type
	ASN1(..),

	-- * marshall an ASN1 type from a val struct or a bytestring
	getEOC,
	getBoolean,
	getInteger,
	getBitString,
	getOctetString,
	getUTF8String,
	getNumericString,
	getPrintableString,
	getT61String,
	getVideoTexString,
	getIA5String,
	getNull,
	getOID,
	getUTCTime,
	getGeneralizedTime,

	-- * marshall an ASN1 type to a bytestring
	putUTCTime,
	putGeneralizedTime,
	putInteger,
	putBitString,
	putString,
	putOID
	) where

import Data.ASN1.Internal
import Data.ASN1.Raw
import Data.Bits
import Data.Word
import Data.Maybe (catMaybes)
import Data.List (unfoldr)
import Data.ByteString (ByteString)
import Data.Char (ord)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

data ASN1 =
	  EOC
	| Boolean Bool
	| IntVal Integer
	| BitString Int L.ByteString
	| OctetString L.ByteString
	| Null
	| OID [Integer]
	| Real Double
	| Enumerated
	| UTF8String L.ByteString
	| Sequence [ASN1]
	| Set [ASN1]
	| NumericString L.ByteString
	| PrintableString L.ByteString
	| T61String L.ByteString
	| VideoTexString L.ByteString
	| IA5String L.ByteString
	| UTCTime (Int, Int, Int, Int, Int, Int, Bool)
	| GeneralizedTime (Int, Int, Int, Int, Int, Int, Bool)
	| GraphicString L.ByteString
	| VisibleString L.ByteString
	| GeneralString L.ByteString
	| UniversalString L.ByteString
	| Other TagClass TagNumber (Either ByteString [ASN1])
	deriving (Show, Eq)

getEOC :: ByteString -> Either ASN1Err ASN1
getEOC s =
	if B.length s == 0
		then Right $ EOC
		else Left $ ASN1Misc "EOC: data length not within bound"

getBoolean :: Bool -> ByteString -> Either ASN1Err ASN1
getBoolean isDer s =
	if B.length s == 1
		then
			case B.head s of
				0    -> Right (Boolean False)
				0xff -> Right (Boolean True)
				_    -> if isDer then Left $ ASN1PolicyFailed "DER" "boolean value not canonical" else Right (Boolean True)
		else Left $ ASN1Misc "boolean: length not within bound"

{- | getInteger, parse a value bytestring and get the integer out of the two complement encoded bytes -}
getInteger :: ByteString -> Either ASN1Err ASN1
getInteger s
	| B.length s == 0 = Left $ ASN1Misc "integer: null encoding"
	| B.length s == 1 = Right $ IntVal $ snd $ intOfBytes s
	| otherwise       =
		if (v1 == 0xff && testBit v2 7) || (v1 == 0x0 && (not $ testBit v2 7))
			then Left $ ASN1Misc "integer: not shortest encoding"
			else Right $ IntVal $ snd $ intOfBytes s
		where
			v1 = s `B.index` 0
			v2 = s `B.index` 1


getBitString :: ValStruct -> Either ASN1Err ASN1
getBitString (Primitive s) =
	let toSkip = B.head s in
	let toSkip' = if toSkip >= 48 && toSkip <= 48 + 7 then toSkip - (fromIntegral $ ord '0') else toSkip in
	let xs = B.tail s in
	if toSkip' >= 0 && toSkip' <= 7
		then Right $ BitString (fromIntegral toSkip') (L.fromChunks [xs])
		else Left $ ASN1Misc ("bitstring: skip number not within bound " ++ show toSkip' ++ " " ++  show s)

getBitString (Constructed _) = Left $ ASN1NotImplemented "bitstring"

getString :: (Either ByteString Value -> Maybe ASN1Err) -> ValStruct -> Either ASN1Err L.ByteString
getString check (Primitive s) =
	case check (Left s) of
		Nothing  -> Right $ L.fromChunks [s]
		Just err -> Left err

getString check (Constructed l) =
	case catMaybes $ map (check . Right) l of
		[]   -> Right $ L.fromChunks $ map catPrimitiveString l
		errs -> Left $ ASN1Multiple errs
	where
		catPrimitiveString (Value _ _ (Primitive b)) = b
		catPrimitiveString (Value _ _ (Constructed _)) = B.empty {- FIXME -}

getOctetString :: ValStruct -> Either ASN1Err ASN1
getOctetString = either Left (Right . OctetString) . getString (\_ -> Nothing)

getNumericString :: ValStruct -> Either ASN1Err ASN1
getNumericString = either Left (Right . NumericString) . getString (\_ -> Nothing)

getPrintableString :: ValStruct -> Either ASN1Err ASN1
getPrintableString = either Left (Right . PrintableString) . getString (\_ -> Nothing)

getUTF8String :: ValStruct -> Either ASN1Err ASN1
getUTF8String = either Left (Right . UTF8String) . getString (\_ -> Nothing)

getT61String :: ValStruct -> Either ASN1Err ASN1
getT61String = either Left (Right . T61String) . getString (\_ -> Nothing)

getVideoTexString :: ValStruct -> Either ASN1Err ASN1
getVideoTexString = either Left (Right . VideoTexString) . getString (\_ -> Nothing)

getIA5String :: ValStruct -> Either ASN1Err ASN1
getIA5String = either Left (Right . IA5String) . getString (\_ -> Nothing)

getNull :: ByteString -> Either ASN1Err ASN1
getNull s = if B.length s == 0 then Right Null else Left $ ASN1Misc "Null: data length not within bound"

{- | return an OID -}
getOID :: ByteString -> Either ASN1Err ASN1
getOID s = Right $ OID $ (fromIntegral (x `div` 40) : fromIntegral (x `mod` 40) : groupOID xs)
	where
		(x:xs) = B.unpack s

		groupOID :: [Word8] -> [Integer]
		groupOID = map (foldl (\acc n -> (acc `shiftL` 7) + fromIntegral n) 0) . groupSubOID

		groupSubOIDHelper [] = Nothing
		groupSubOIDHelper l  = Just $ spanSubOIDbound l

		groupSubOID :: [Word8] -> [[Word8]]
		groupSubOID = unfoldr groupSubOIDHelper

		spanSubOIDbound [] = ([], [])
		spanSubOIDbound (a:as) = if testBit a 7 then (clearBit a 7 : ys, zs) else ([a], as)
			where (ys, zs) = spanSubOIDbound as

getUTCTime :: ValStruct -> Either ASN1Err ASN1
getUTCTime (Primitive s) =
	case B.unpack s of
		[y1, y2, m1, m2, d1, d2, h1, h2, mi1, mi2, s1, s2, z] ->
			let y = integerise y1 y2 in
			let year = 1900 + (if y <= 50 then y + 100 else y) in
			let month = integerise m1 m2 in
			let day = integerise d1 d2 in
			let hour = integerise h1 h2 in
			let minute = integerise mi1 mi2 in
			let second = integerise s1 s2 in
			Right $ UTCTime (year, month, day, hour, minute, second, z == 90)
		_                                                     -> Left $ ASN1Misc "utctime unexpected format"
	where
		integerise a b = ((fromIntegral a) - (ord '0')) * 10 + ((fromIntegral b) - (ord '0'))

getUTCTime (Constructed _) = Left $ ASN1NotImplemented "utctime constructed"

getGeneralizedTime :: ValStruct -> Either ASN1Err ASN1
getGeneralizedTime (Primitive s) =
	case B.unpack s of
		[y1, y2, y3, y4, m1, m2, d1, d2, h1, h2, mi1, mi2, s1, s2, z] ->
			let year = (integerise y1 y2) * 100 + (integerise y3 y4) in
			let month = integerise m1 m2 in
			let day = integerise d1 d2 in
			let hour = integerise h1 h2 in
			let minute = integerise mi1 mi2 in
			let second = integerise s1 s2 in
			Right $ GeneralizedTime (year, month, day, hour, minute, second, z == 90)
		_                                                     -> Left $ ASN1Misc "utctime unexpected format"
	where
		integerise a b = ((fromIntegral a) - (ord '0')) * 10 + ((fromIntegral b) - (ord '0'))
getGeneralizedTime (Constructed _) = Left $ ASN1NotImplemented "generalizedtime constructed"

putTime :: Bool -> (Int, Int, Int, Int, Int, Int, Bool) -> ValStruct
putTime generalized (y,m,d,h,mi,s,z) =
	Primitive $ B.pack etime
	where
		etime =
			if generalized
				then [y1, y2, y3, y4, m1, m2, d1, d2, h1, h2, mi1, mi2, s1, s2, if z then 90 else 0 ]
				else [y3, y4, m1, m2, d1, d2, h1, h2, mi1, mi2, s1, s2, if z then 90 else 0 ]
		split2 n          = (fromIntegral $ n `div` 10 + ord '0', fromIntegral $ n `mod` 10 + ord '0')
		((y1,y2),(y3,y4)) = (split2 (y `div` 100), split2 (y `mod` 100))
		(m1, m2)          = split2 m
		(d1, d2)          = split2 d
		(h1, h2)          = split2 h
		(mi1, mi2)        = split2 mi
		(s1, s2)          = split2 s

putUTCTime :: (Int, Int, Int, Int, Int, Int, Bool) -> ValStruct
putUTCTime time = putTime False time

putGeneralizedTime :: (Int, Int, Int, Int, Int, Int, Bool) -> ValStruct
putGeneralizedTime time = putTime True time

putInteger :: Integer -> ValStruct
putInteger i = Primitive $ B.pack $ bytesOfInt i

putBitString :: Int -> L.ByteString -> ValStruct
putBitString i bits = Primitive $ B.concat $ B.singleton (fromIntegral i) : L.toChunks bits

putString :: L.ByteString -> ValStruct
putString l = Primitive $ B.concat $ L.toChunks l

{- no enforce check that we oid1 is between [0..2] and oid2 is between [0..39] -}
putOID :: [Integer] -> ValStruct
putOID oids = Primitive $ B.pack $ eoid
	where
		(oid1:oid2:suboids) = oids
		eoidclass           = fromIntegral (oid1 * 40 + oid2)
		ungroupSubOID x     = unfoldr (\i -> if i == 0 then Nothing else Just (fromIntegral (i .&. 0x7f), i `shiftR` 7)) x
		setHighBits []      = []
		setHighBits [x]     = [x]
		setHighBits (x:xs)  = setBit x 7 : setHighBits xs
		subeoids            = concatMap (setHighBits . reverse . ungroupSubOID) suboids
		eoid                = eoidclass : subeoids
