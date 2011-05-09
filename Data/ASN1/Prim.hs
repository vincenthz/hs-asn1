-- |
-- Module      : Data.ASN1.Prim
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Tools to read ASN1 primitive (e.g. boolean, int)
--

module Data.ASN1.Prim
	(
	-- * ASN1 encoding and decoding
	  decodeUCS2BE
	, decodeUtf8
	, decodeUtf32BE
	, decodeASCII
	, encodeUCS2BE
	, encodeUtf8
	, encodeUtf32BE
	, encodeASCII

	-- * ASN1 utility to encode length
	, mkSmallestLength

	-- * marshall specific asn1 bytestring into a typed value
	, getBoolean
	, getInteger
	, getBitString
	, getNull
	, getOID
	, getUTCTime
	, getGeneralizedTime
	, getString

	-- * marshall an typed value into a bytestring
	, putUTCTime
	, putGeneralizedTime
	, putInteger
	, putBitString
	, putString
	, putOID
	) where

import Data.ASN1.Internal
import Data.ASN1.Event
import Data.Bits
import Data.Word
import Data.List (unfoldr)
import Data.ByteString (ByteString)
import Data.Char (ord)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

encodeUCS2BE :: String -> ByteString
encodeUCS2BE t =
	B.pack $ concatMap (\c -> let (d,m) = (fromEnum c) `divMod` 256 in [fromIntegral m,fromIntegral d]) t

decodeUCS2BE :: ByteString -> String
decodeUCS2BE = loop where
	loop x
		| B.null x  = []
		| otherwise =
			let (h, r) = B.splitAt 2 x in
			case B.unpack h of
				[a,b] -> (toEnum $ (fromIntegral a) + (fromIntegral b) * 256) : loop r
				_     -> loop r

decodeASCII :: ByteString -> String
decodeASCII = T.unpack . T.decodeASCII

encodeASCII :: String -> ByteString
encodeASCII = T.encodeUtf8 . T.pack

decodeUtf8 :: ByteString -> String
decodeUtf8 = T.unpack . T.decodeUtf8

encodeUtf8 :: String -> ByteString
encodeUtf8 = T.encodeUtf8 . T.pack

decodeUtf32BE :: ByteString -> String
decodeUtf32BE = T.unpack . T.decodeUtf32BE

encodeUtf32BE :: String -> ByteString
encodeUtf32BE = T.encodeUtf32BE . T.pack

getBoolean :: Bool -> ByteString -> Either ASN1Err Bool
getBoolean isDer s
	| B.length s /= 1  = Left $ ASN1Misc "boolean: length not within bound"
	| B.head s == 0    = Right False
	| B.head s == 0xff = Right True
	| otherwise        = if isDer
		then Left $ ASN1PolicyFailed "DER" "boolean value not canonical"
		else Right True

{- | getInteger, parse a value bytestring and get the integer out of the two complement encoded bytes -}
getInteger :: ByteString -> Either ASN1Err Integer
getInteger s
	| B.length s == 0 = Left $ ASN1Misc "integer: null encoding"
	| B.length s == 1 = Right $ snd $ intOfBytes s
	| otherwise       =
		if (v1 == 0xff && testBit v2 7) || (v1 == 0x0 && (not $ testBit v2 7))
			then Left $ ASN1Misc "integer: not shortest encoding"
			else Right $ snd $ intOfBytes s
		where
			v1 = s `B.index` 0
			v2 = s `B.index` 1

getBitString :: ByteString -> Either ASN1Err (Int, B.ByteString)
getBitString s =
	let toSkip = B.head s in
	let toSkip' = if toSkip >= 48 && toSkip <= 48 + 7 then toSkip - (fromIntegral $ ord '0') else toSkip in
	let xs = B.tail s in
	if toSkip' >= 0 && toSkip' <= 7
		then Right $ (fromIntegral toSkip', xs)
		else Left $ ASN1Misc ("bitstring: skip number not within bound " ++ show toSkip' ++ " " ++  show s)

getString :: (ByteString -> a) -> ByteString -> Either ASN1Err a
getString mapS s = Right $ mapS s

getNull :: ByteString -> Either ASN1Err ()
getNull s = if B.length s == 0 then Right () else Left $ ASN1Misc "Null: data length not within bound"

{- | return an OID -}
getOID :: ByteString -> Either ASN1Err [Integer]
getOID s = Right $ (fromIntegral (x `div` 40) : fromIntegral (x `mod` 40) : groupOID xs)
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

getUTCTime :: ByteString -> Either ASN1Err (Int, Int, Int, Int, Int, Int, Bool)
getUTCTime s =
	case B.unpack s of
		[y1, y2, m1, m2, d1, d2, h1, h2, mi1, mi2, s1, s2, z] ->
			let y = integerise y1 y2 in
			let year = 1900 + (if y <= 50 then y + 100 else y) in
			let month = integerise m1 m2 in
			let day = integerise d1 d2 in
			let hour = integerise h1 h2 in
			let minute = integerise mi1 mi2 in
			let second = integerise s1 s2 in
			Right $ (year, month, day, hour, minute, second, z == 90)
		_                                                     -> Left $ ASN1Misc "utctime unexpected format"
	where
		integerise a b = ((fromIntegral a) - (ord '0')) * 10 + ((fromIntegral b) - (ord '0'))

getGeneralizedTime :: ByteString -> Either ASN1Err (Int, Int, Int, Int, Int, Int, Bool)
getGeneralizedTime s =
	case B.unpack s of
		[y1, y2, y3, y4, m1, m2, d1, d2, h1, h2, mi1, mi2, s1, s2, z] ->
			let year = (integerise y1 y2) * 100 + (integerise y3 y4) in
			let month = integerise m1 m2 in
			let day = integerise d1 d2 in
			let hour = integerise h1 h2 in
			let minute = integerise mi1 mi2 in
			let second = integerise s1 s2 in
			Right $ (year, month, day, hour, minute, second, z == 90)
		_                                                     -> Left $ ASN1Misc "utctime unexpected format"
	where
		integerise a b = ((fromIntegral a) - (ord '0')) * 10 + ((fromIntegral b) - (ord '0'))

putTime :: Bool -> (Int, Int, Int, Int, Int, Int, Bool) -> ByteString
putTime generalized (y,m,d,h,mi,s,z) = B.pack etime
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

putUTCTime :: (Int, Int, Int, Int, Int, Int, Bool) -> ByteString
putUTCTime time = putTime False time

putGeneralizedTime :: (Int, Int, Int, Int, Int, Int, Bool) -> ByteString
putGeneralizedTime time = putTime True time

putInteger :: Integer -> ByteString
putInteger i = B.pack $ bytesOfInt i

putBitString :: Int -> ByteString -> ByteString
putBitString i bits = B.singleton (fromIntegral i) `B.append` bits

putString :: ByteString -> ByteString
putString l = l

{- no enforce check that oid1 is between [0..2] and oid2 is between [0..39] -}
putOID :: [Integer] -> ByteString
putOID oids = B.cons eoidclass subeoids
	where
		(oid1:oid2:suboids) = oids
		eoidclass           = fromIntegral (oid1 * 40 + oid2)
		encode x | x == 0    = B.singleton 0
		       	 | otherwise = putVarEncodingIntegral x
		subeoids  = B.concat $ map encode suboids

mkSmallestLength :: Int -> ASN1Length
mkSmallestLength i
	| i < 0x80  = LenShort i
	| otherwise = LenLong (nbBytes i) i
		where nbBytes nb = if nb > 255 then 1 + nbBytes (nb `div` 256) else 1
