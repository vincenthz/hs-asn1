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
	-- * ASN1 high level algebraic type
	  ASN1(..)
	, ConstructionType(..)

	, encodeHeader
	, encodePrimitiveHeader
	, encodePrimitive
	, decodePrimitive

	-- * marshall an ASN1 type from a val struct or a bytestring
	, getBoolean
	, getInteger
	, getBitString
	, getOctetString
	, getUTF8String
	, getNumericString
	, getPrintableString
	, getT61String
	, getVideoTexString
	, getIA5String
	, getNull
	, getOID
	, getUTCTime
	, getGeneralizedTime
	, getGraphicString
	, getVisibleString
	, getGeneralString
	, getUniversalString
	, getCharacterString
	, getBMPString

	-- * marshall an ASN1 type to a bytestring
	, putUTCTime
	, putGeneralizedTime
	, putInteger
	, putBitString
	, putString
	, putOID
	) where

import Data.ASN1.Internal
import Data.ASN1.Raw
import Data.ASN1.Stream
import Data.Bits
import Data.Word
import Data.List (unfoldr)
import Data.ByteString (ByteString)
import Data.Char (ord)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeASCII, decodeUtf8, decodeUtf32BE, encodeUtf8, encodeUtf32BE)

encodeUCS2BE :: Text -> L.ByteString
encodeUCS2BE t =
	L.pack $ concatMap (\c -> let (d,m) = (fromEnum c) `divMod` 256 in [fromIntegral m,fromIntegral d] ) $ T.unpack t

decodeUCS2BE :: L.ByteString -> Text
decodeUCS2BE l = T.pack $ loop l
	where
		loop x
			| L.null x  = []
			| otherwise = 
				let (h, r) = L.splitAt 2 l in
				case L.unpack h of
					[a,b] -> (toEnum $ (fromIntegral a) + (fromIntegral b) * 256) : loop r
					_     -> loop r
	

encodeHeader :: Bool -> ASN1Length -> ASN1 -> ASN1Header
encodeHeader pc len (Boolean _)                = ASN1Header Universal 0x1 pc len
encodeHeader pc len (IntVal _)                 = ASN1Header Universal 0x2 pc len
encodeHeader pc len (BitString _ _)            = ASN1Header Universal 0x3 pc len
encodeHeader pc len (OctetString _)            = ASN1Header Universal 0x4 pc len
encodeHeader pc len Null                       = ASN1Header Universal 0x5 pc len
encodeHeader pc len (OID _)                    = ASN1Header Universal 0x6 pc len
encodeHeader pc len (Real _)                   = ASN1Header Universal 0x9 pc len
encodeHeader pc len Enumerated                 = ASN1Header Universal 0xa pc len
encodeHeader pc len (UTF8String _)             = ASN1Header Universal 0xc pc len
encodeHeader pc len (NumericString _)          = ASN1Header Universal 0x12 pc len
encodeHeader pc len (PrintableString _)        = ASN1Header Universal 0x13 pc len
encodeHeader pc len (T61String _)              = ASN1Header Universal 0x14 pc len
encodeHeader pc len (VideoTexString _)         = ASN1Header Universal 0x15 pc len
encodeHeader pc len (IA5String _)              = ASN1Header Universal 0x16 pc len
encodeHeader pc len (UTCTime _)                = ASN1Header Universal 0x17 pc len
encodeHeader pc len (GeneralizedTime _)        = ASN1Header Universal 0x18 pc len
encodeHeader pc len (GraphicString _)          = ASN1Header Universal 0x19 pc len
encodeHeader pc len (VisibleString _)          = ASN1Header Universal 0x1a pc len
encodeHeader pc len (GeneralString _)          = ASN1Header Universal 0x1b pc len
encodeHeader pc len (UniversalString _)        = ASN1Header Universal 0x1c pc len
encodeHeader pc len (CharacterString _)        = ASN1Header Universal 0x1d pc len
encodeHeader pc len (BMPString _)              = ASN1Header Universal 0x1e pc len
encodeHeader pc len (Start Sequence)           = ASN1Header Universal 0x10 pc len
encodeHeader pc len (Start Set)                = ASN1Header Universal 0x11 pc len
encodeHeader pc len (Start (Container tc tag)) = ASN1Header tc tag pc len
encodeHeader pc len (Other tc tag _)           = ASN1Header tc tag pc len
encodeHeader _ _ (End _)                       = error "this should not happen"

encodePrimitiveHeader :: ASN1Length -> ASN1 -> ASN1Header
encodePrimitiveHeader = encodeHeader False

encodePrimitiveData :: ASN1 -> ByteString
encodePrimitiveData (Boolean b)         = B.singleton (if b then 0xff else 0)
encodePrimitiveData (IntVal i)          = putInteger i
encodePrimitiveData (BitString i bits)  = putBitString i bits
encodePrimitiveData (OctetString b)     = putString b
encodePrimitiveData Null                = B.empty
encodePrimitiveData (OID oid)           = putOID oid
encodePrimitiveData (Real _)            = B.empty -- not implemented
encodePrimitiveData Enumerated          = B.empty -- not implemented
encodePrimitiveData (UTF8String b)      = putString $ encodeUtf8 b
encodePrimitiveData (NumericString b)   = putString b
encodePrimitiveData (PrintableString b) = putString $ encodeUtf8 b
encodePrimitiveData (T61String b)       = putString b
encodePrimitiveData (VideoTexString b)  = putString b
encodePrimitiveData (IA5String b)       = putString $ encodeUtf8 b
encodePrimitiveData (UTCTime t)         = putUTCTime t
encodePrimitiveData (GeneralizedTime t) = putGeneralizedTime t
encodePrimitiveData (GraphicString b)   = putString b
encodePrimitiveData (VisibleString b)   = putString b
encodePrimitiveData (GeneralString b)   = putString b
encodePrimitiveData (UniversalString b) = putString $ encodeUtf32BE b
encodePrimitiveData (CharacterString b) = putString b
encodePrimitiveData (BMPString b)       = putString $ encodeUCS2BE b
encodePrimitiveData (Other _ _ b)       = b
encodePrimitiveData _                   = error "not a primitive"

encodePrimitive :: ASN1 -> (ASN1Header, ByteString)
encodePrimitive a =
	let b = encodePrimitiveData a in
	let len = makeLength (B.length b) in
	(encodePrimitiveHeader len a, b)
	where
		makeLength len
			| len < 0x80 = LenShort len
			| otherwise  = LenLong (nbBytes len) len
		nbBytes nb = if nb > 255 then 1 + nbBytes (nb `div` 256) else 1

type ASN1Ret = Either ASN1Err ASN1

decodePrimitive :: ASN1Header -> B.ByteString -> ASN1Ret
decodePrimitive (ASN1Header Universal 0x1 _ _) p   = getBoolean False p
decodePrimitive (ASN1Header Universal 0x2 _ _) p   = getInteger p
decodePrimitive (ASN1Header Universal 0x3 _ _) p   = getBitString p
decodePrimitive (ASN1Header Universal 0x4 _ _) p   = getOctetString p
decodePrimitive (ASN1Header Universal 0x5 _ _) p   = getNull p
decodePrimitive (ASN1Header Universal 0x6 _ _) p   = getOID p
decodePrimitive (ASN1Header Universal 0x7 _ _) _   = Left $ ASN1NotImplemented "Object Descriptor"
decodePrimitive (ASN1Header Universal 0x8 _ _) _   = Left $ ASN1NotImplemented "External"
decodePrimitive (ASN1Header Universal 0x9 _ _) _   = Left $ ASN1NotImplemented "real"
decodePrimitive (ASN1Header Universal 0xa _ _) _   = Left $ ASN1NotImplemented "enumerated"
decodePrimitive (ASN1Header Universal 0xb _ _) _   = Left $ ASN1NotImplemented "EMBEDDED PDV"
decodePrimitive (ASN1Header Universal 0xc _ _) p   = getUTF8String p
decodePrimitive (ASN1Header Universal 0xd _ _) _   = Left $ ASN1NotImplemented "RELATIVE-OID"
decodePrimitive (ASN1Header Universal 0x10 _ _) _  = error "sequence not a primitive"
decodePrimitive (ASN1Header Universal 0x11 _ _) _  = error "set not a primitive"
decodePrimitive (ASN1Header Universal 0x12 _ _) p  = getNumericString p
decodePrimitive (ASN1Header Universal 0x13 _ _) p  = getPrintableString p
decodePrimitive (ASN1Header Universal 0x14 _ _) p  = getT61String p
decodePrimitive (ASN1Header Universal 0x15 _ _) p  = getVideoTexString p
decodePrimitive (ASN1Header Universal 0x16 _ _) p  = getIA5String p
decodePrimitive (ASN1Header Universal 0x17 _ _) p  = getUTCTime p
decodePrimitive (ASN1Header Universal 0x18 _ _) p  = getGeneralizedTime p
decodePrimitive (ASN1Header Universal 0x19 _ _) p  = getGraphicString p
decodePrimitive (ASN1Header Universal 0x1a _ _) p  = getVisibleString p
decodePrimitive (ASN1Header Universal 0x1b _ _) p  = getGeneralString p
decodePrimitive (ASN1Header Universal 0x1c _ _) p  = getUniversalString p
decodePrimitive (ASN1Header Universal 0x1d _ _) p  = getCharacterString p
decodePrimitive (ASN1Header Universal 0x1e _ _) p  = getBMPString p
decodePrimitive (ASN1Header tc        tag  _ _) p  = Right $ Other tc tag p


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


getBitString :: ByteString -> Either ASN1Err ASN1
getBitString s =
	let toSkip = B.head s in
	let toSkip' = if toSkip >= 48 && toSkip <= 48 + 7 then toSkip - (fromIntegral $ ord '0') else toSkip in
	let xs = B.tail s in
	if toSkip' >= 0 && toSkip' <= 7
		then Right $ BitString (fromIntegral toSkip') (L.fromChunks [xs])
		else Left $ ASN1Misc ("bitstring: skip number not within bound " ++ show toSkip' ++ " " ++  show s)

getString :: (ByteString -> Maybe ASN1Err) -> ByteString -> Either ASN1Err L.ByteString
getString check s =
	case check s of
		Nothing  -> Right $ L.fromChunks [s]
		Just err -> Left err

getOctetString :: ByteString -> Either ASN1Err ASN1
getOctetString = either Left (Right . OctetString) . getString (\_ -> Nothing)

getNumericString :: ByteString -> Either ASN1Err ASN1
getNumericString = either Left (Right . NumericString) . getString (\_ -> Nothing)

getPrintableString :: ByteString -> Either ASN1Err ASN1
getPrintableString = either Left (Right . PrintableString . decodeASCII) . getString (\_ -> Nothing)

getUTF8String :: ByteString -> Either ASN1Err ASN1
getUTF8String = either Left (Right . UTF8String . decodeUtf8) . getString (\_ -> Nothing)

getT61String :: ByteString -> Either ASN1Err ASN1
getT61String = either Left (Right . T61String) . getString (\_ -> Nothing)

getVideoTexString :: ByteString -> Either ASN1Err ASN1
getVideoTexString = either Left (Right . VideoTexString) . getString (\_ -> Nothing)

getIA5String :: ByteString -> Either ASN1Err ASN1
getIA5String = either Left (Right . IA5String . decodeASCII) . getString (\_ -> Nothing)

getGraphicString :: ByteString -> Either ASN1Err ASN1
getGraphicString = either Left (Right . GraphicString) . getString (\_ -> Nothing)

getVisibleString :: ByteString -> Either ASN1Err ASN1
getVisibleString = either Left (Right . VisibleString) . getString (\_ -> Nothing)

getGeneralString :: ByteString -> Either ASN1Err ASN1
getGeneralString = either Left (Right . GeneralString) . getString (\_ -> Nothing)

getUniversalString :: ByteString -> Either ASN1Err ASN1
getUniversalString = either Left (Right . UniversalString . decodeUtf32BE) . getString (\_ -> Nothing)

getCharacterString :: ByteString -> Either ASN1Err ASN1
getCharacterString = either Left (Right . CharacterString) . getString (\_ -> Nothing)

getBMPString :: ByteString -> Either ASN1Err ASN1
getBMPString = either Left (Right . BMPString . decodeUCS2BE) . getString (\_ -> Nothing)

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

getUTCTime :: ByteString -> Either ASN1Err ASN1
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
			Right $ UTCTime (year, month, day, hour, minute, second, z == 90)
		_                                                     -> Left $ ASN1Misc "utctime unexpected format"
	where
		integerise a b = ((fromIntegral a) - (ord '0')) * 10 + ((fromIntegral b) - (ord '0'))

getGeneralizedTime :: ByteString -> Either ASN1Err ASN1
getGeneralizedTime s =
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

putBitString :: Int -> L.ByteString -> ByteString
putBitString i bits = B.concat $ B.singleton (fromIntegral i) : L.toChunks bits

putString :: L.ByteString -> ByteString
putString l = B.concat $ L.toChunks l

{- no enforce check that oid1 is between [0..2] and oid2 is between [0..39] -}
putOID :: [Integer] -> ByteString
putOID oids = B.pack $ eoid
	where
		(oid1:oid2:suboids) = oids
		eoidclass           = fromIntegral (oid1 * 40 + oid2)
		ungroupSubOID x     = unfoldr (\i -> if i == 0 then Nothing else Just (fromIntegral (i .&. 0x7f), i `shiftR` 7)) x
		setHighBits []      = []
		setHighBits [x]     = [x]
		setHighBits (x:xs)  = setBit x 7 : setHighBits xs
		subeoids            = concatMap (setHighBits . reverse . ungroupSubOID) suboids
		eoid                = eoidclass : subeoids
