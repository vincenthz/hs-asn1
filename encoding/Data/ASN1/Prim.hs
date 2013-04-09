-- |
-- Module      : Data.ASN1.Prim
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Tools to read ASN1 primitive (e.g. boolean, int)
--

{-# LANGUAGE ViewPatterns #-}
module Data.ASN1.Prim
	(
	-- * ASN1 high level algebraic type
	  ASN1(..)
	, ASN1ConstructionType(..)

	, encodeHeader
	, encodePrimitiveHeader
	, encodePrimitive
	, decodePrimitive
	, encodeConstructed
	, encodeList
	, encodeOne
	, mkSmallestLength

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
	, getTime
	, getGraphicString
	, getVisibleString
	, getGeneralString
	, getUniversalString
	, getCharacterString
	, getBMPString

	-- * marshall an ASN1 type to a bytestring
	, putTime
	, putInteger
	, putBitString
	, putString
	, putOID
	) where

import Data.ASN1.Internal
import Data.ASN1.Stream
import Data.ASN1.BitArray
import Data.ASN1.Types
import Data.ASN1.Types.Lowlevel
import Data.ASN1.Error
import Data.ASN1.Serialize
import Data.Bits
import Data.Word
import Data.List (unfoldr)
import Data.ByteString (ByteString)
import Data.Char (ord)
import qualified Data.ByteString as B
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Control.Arrow (first)
import Control.Applicative

encodeHeader :: Bool -> ASN1Length -> ASN1 -> ASN1Header
encodeHeader pc len (Boolean _)                = ASN1Header Universal 0x1 pc len
encodeHeader pc len (IntVal _)                 = ASN1Header Universal 0x2 pc len
encodeHeader pc len (BitString _)              = ASN1Header Universal 0x3 pc len
encodeHeader pc len (OctetString _)            = ASN1Header Universal 0x4 pc len
encodeHeader pc len Null                       = ASN1Header Universal 0x5 pc len
encodeHeader pc len (OID _)                    = ASN1Header Universal 0x6 pc len
encodeHeader pc len (Real _)                   = ASN1Header Universal 0x9 pc len
encodeHeader pc len (Enumerated _)             = ASN1Header Universal 0xa pc len
encodeHeader pc len (ASN1String UTF8 _)        = ASN1Header Universal 0xc pc len
encodeHeader pc len (ASN1String Numeric _)     = ASN1Header Universal 0x12 pc len
encodeHeader pc len (ASN1String Printable _)   = ASN1Header Universal 0x13 pc len
encodeHeader pc len (ASN1String T61 _)         = ASN1Header Universal 0x14 pc len
encodeHeader pc len (ASN1String VideoTex _)    = ASN1Header Universal 0x15 pc len
encodeHeader pc len (ASN1String IA5 _)         = ASN1Header Universal 0x16 pc len
encodeHeader pc len (ASN1Time TimeUTC _ _)     = ASN1Header Universal 0x17 pc len
encodeHeader pc len (ASN1Time TimeGeneralized _ _) = ASN1Header Universal 0x18 pc len
encodeHeader pc len (ASN1String Graphic _)     = ASN1Header Universal 0x19 pc len
encodeHeader pc len (ASN1String Visible _)     = ASN1Header Universal 0x1a pc len
encodeHeader pc len (ASN1String General _)     = ASN1Header Universal 0x1b pc len
encodeHeader pc len (ASN1String UTF32 _)       = ASN1Header Universal 0x1c pc len
encodeHeader pc len (ASN1String Character _)   = ASN1Header Universal 0x1d pc len
encodeHeader pc len (ASN1String BMP _)         = ASN1Header Universal 0x1e pc len
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
encodePrimitiveData (BitString bits)    = putBitString bits
encodePrimitiveData (OctetString b)     = putString b
encodePrimitiveData Null                = B.empty
encodePrimitiveData (OID oidv)          = putOID oidv
encodePrimitiveData (Real _)            = B.empty -- not implemented
encodePrimitiveData (Enumerated i)      = putInteger $ fromIntegral i
encodePrimitiveData (ASN1String _ b)    = b
encodePrimitiveData (ASN1Time ty ti tz) = putTime ty ti tz
encodePrimitiveData (Other _ _ b)       = b
encodePrimitiveData o                   = error ("not a primitive " ++ show o)

encodePrimitive :: ASN1 -> (Int, [ASN1Event])
encodePrimitive a =
	let b = encodePrimitiveData a in
	let blen = B.length b in
	let len = makeLength blen in
	let hdr = encodePrimitiveHeader len a in
	(B.length (putHeader hdr) + blen, [Header hdr, Primitive b])
	where
		makeLength len
			| len < 0x80 = LenShort len
			| otherwise  = LenLong (nbBytes len) len
		nbBytes nb = if nb > 255 then 1 + nbBytes (nb `div` 256) else 1

encodeOne :: ASN1 -> (Int, [ASN1Event])
encodeOne (Start _) = error "encode one cannot do start"
encodeOne t         = encodePrimitive t

encodeList :: [ASN1] -> (Int, [ASN1Event])
encodeList []               = (0, [])
encodeList (End _:xs)       = encodeList xs
encodeList (t@(Start _):xs) =
	let (ys, zs)    = getConstructedEnd 0 xs in
	let (llen, lev) = encodeList zs in
	let (len, ev)   = encodeConstructed t ys in
	(llen + len, ev ++ lev)

encodeList (x:xs)           =
	let (llen, lev) = encodeList xs in
	let (len, ev)   = encodeOne x in
	(llen + len, ev ++ lev)

encodeConstructed :: ASN1 -> [ASN1] -> (Int, [ASN1Event])
encodeConstructed c@(Start _) children =
	let (clen, events) = encodeList children in
	let len = mkSmallestLength clen in
	let h = encodeHeader True len c in
	let tlen = B.length (putHeader h) + clen in
	(tlen, Header h : ConstructionBegin : events ++ [ConstructionEnd])

encodeConstructed _ _ = error "not a start node"

mkSmallestLength :: Int -> ASN1Length
mkSmallestLength i
	| i < 0x80  = LenShort i
	| otherwise = LenLong (nbBytes i) i
		where nbBytes nb = if nb > 255 then 1 + nbBytes (nb `div` 256) else 1

type ASN1Ret = Either ASN1Error ASN1

decodePrimitive :: ASN1Header -> B.ByteString -> ASN1Ret
decodePrimitive (ASN1Header Universal 0x1 _ _) p   = getBoolean False p
decodePrimitive (ASN1Header Universal 0x2 _ _) p   = getInteger p
decodePrimitive (ASN1Header Universal 0x3 _ _) p   = getBitString p
decodePrimitive (ASN1Header Universal 0x4 _ _) p   = getOctetString p
decodePrimitive (ASN1Header Universal 0x5 _ _) p   = getNull p
decodePrimitive (ASN1Header Universal 0x6 _ _) p   = getOID p
decodePrimitive (ASN1Header Universal 0x7 _ _) _   = Left $ TypeNotImplemented "Object Descriptor"
decodePrimitive (ASN1Header Universal 0x8 _ _) _   = Left $ TypeNotImplemented "External"
decodePrimitive (ASN1Header Universal 0x9 _ _) _   = Left $ TypeNotImplemented "real"
decodePrimitive (ASN1Header Universal 0xa _ _) _   = Left $ TypeNotImplemented "enumerated"
decodePrimitive (ASN1Header Universal 0xb _ _) _   = Left $ TypeNotImplemented "EMBEDDED PDV"
decodePrimitive (ASN1Header Universal 0xc _ _) p   = getUTF8String p
decodePrimitive (ASN1Header Universal 0xd _ _) _   = Left $ TypeNotImplemented "RELATIVE-OID"
decodePrimitive (ASN1Header Universal 0x10 _ _) _  = error "sequence not a primitive"
decodePrimitive (ASN1Header Universal 0x11 _ _) _  = error "set not a primitive"
decodePrimitive (ASN1Header Universal 0x12 _ _) p  = getNumericString p
decodePrimitive (ASN1Header Universal 0x13 _ _) p  = getPrintableString p
decodePrimitive (ASN1Header Universal 0x14 _ _) p  = getT61String p
decodePrimitive (ASN1Header Universal 0x15 _ _) p  = getVideoTexString p
decodePrimitive (ASN1Header Universal 0x16 _ _) p  = getIA5String p
decodePrimitive (ASN1Header Universal 0x17 _ _) p  = getTime TimeUTC p
decodePrimitive (ASN1Header Universal 0x18 _ _) p  = getTime TimeGeneralized p
decodePrimitive (ASN1Header Universal 0x19 _ _) p  = getGraphicString p
decodePrimitive (ASN1Header Universal 0x1a _ _) p  = getVisibleString p
decodePrimitive (ASN1Header Universal 0x1b _ _) p  = getGeneralString p
decodePrimitive (ASN1Header Universal 0x1c _ _) p  = getUniversalString p
decodePrimitive (ASN1Header Universal 0x1d _ _) p  = getCharacterString p
decodePrimitive (ASN1Header Universal 0x1e _ _) p  = getBMPString p
decodePrimitive (ASN1Header tc        tag  _ _) p  = Right $ Other tc tag p


getBoolean :: Bool -> ByteString -> Either ASN1Error ASN1
getBoolean isDer s =
	if B.length s == 1
		then case B.head s of
			0    -> Right (Boolean False)
			0xff -> Right (Boolean True)
			_    -> if isDer then Left $ PolicyFailed "DER" "boolean value not canonical" else Right (Boolean True)
		else Left $ TypeDecodingFailed "boolean: length not within bound"

{- | getInteger, parse a value bytestring and get the integer out of the two complement encoded bytes -}
getInteger :: ByteString -> Either ASN1Error ASN1
getInteger s
	| B.length s == 0 = Left $ TypeDecodingFailed "integer: null encoding"
	| B.length s == 1 = Right $ IntVal $ snd $ intOfBytes s
	| otherwise       =
		if (v1 == 0xff && testBit v2 7) || (v1 == 0x0 && (not $ testBit v2 7))
			then Left $ TypeDecodingFailed "integer: not shortest encoding"
			else Right $ IntVal $ snd $ intOfBytes s
		where
			v1 = s `B.index` 0
			v2 = s `B.index` 1


getBitString :: ByteString -> Either ASN1Error ASN1
getBitString s =
	let toSkip = B.head s in
	let toSkip' = if toSkip >= 48 && toSkip <= 48 + 7 then toSkip - (fromIntegral $ ord '0') else toSkip in
	let xs = B.tail s in
	if toSkip' >= 0 && toSkip' <= 7
		then Right $ BitString $ toBitArray xs (fromIntegral toSkip')
		else Left $ TypeDecodingFailed ("bitstring: skip number not within bound " ++ show toSkip' ++ " " ++  show s)

getString :: (ByteString -> Maybe ASN1Error) -> ByteString -> Either ASN1Error ByteString
getString check s =
	case check s of
		Nothing  -> Right s
		Just err -> Left err

getOctetString :: ByteString -> Either ASN1Error ASN1
getOctetString = Right . OctetString

getNumericString :: ByteString -> Either ASN1Error ASN1
getNumericString = (ASN1String Numeric <$>) . getString (\_ -> Nothing)

getPrintableString :: ByteString -> Either ASN1Error ASN1
getPrintableString = (ASN1String Printable <$>) . getString (\_ -> Nothing)

getUTF8String :: ByteString -> Either ASN1Error ASN1
getUTF8String = (ASN1String UTF8 <$>) . getString (\_ -> Nothing)

getT61String :: ByteString -> Either ASN1Error ASN1
getT61String = (ASN1String T61 <$>) . getString (\_ -> Nothing)

getVideoTexString :: ByteString -> Either ASN1Error ASN1
getVideoTexString = (ASN1String VideoTex <$>) . getString (\_ -> Nothing)

getIA5String :: ByteString -> Either ASN1Error ASN1
getIA5String = (ASN1String IA5 <$>) . getString (\_ -> Nothing)

getGraphicString :: ByteString -> Either ASN1Error ASN1
getGraphicString = (ASN1String Graphic <$>) . getString (\_ -> Nothing)

getVisibleString :: ByteString -> Either ASN1Error ASN1
getVisibleString = (ASN1String Visible <$>) . getString (\_ -> Nothing)

getGeneralString :: ByteString -> Either ASN1Error ASN1
getGeneralString = (ASN1String General <$>) . getString (\_ -> Nothing)

getUniversalString :: ByteString -> Either ASN1Error ASN1
getUniversalString = (ASN1String UTF32 <$>) . getString (\_ -> Nothing)

getCharacterString :: ByteString -> Either ASN1Error ASN1
getCharacterString = (ASN1String Character <$>) . getString (\_ -> Nothing)

getBMPString :: ByteString -> Either ASN1Error ASN1
getBMPString = (ASN1String BMP <$>) . getString (\_ -> Nothing)

getNull :: ByteString -> Either ASN1Error ASN1
getNull s
    | B.length s == 0 = Right Null
    | otherwise       = Left $ TypeDecodingFailed "Null: data length not within bound"

{- | return an OID -}
getOID :: ByteString -> Either ASN1Error ASN1
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

getTime :: ASN1TimeType -> ByteString -> Either ASN1Error ASN1
getTime timeType (B.unpack -> b) = Right $ ASN1Time timeType (UTCTime cDay cDiffTime) tz
    where
          cDay      = fromGregorian year (fromIntegral month) (fromIntegral day)
          cDiffTime = secondsToDiffTime (hour * 3600 + minute * 60 + sec) +
                      picosecondsToDiffTime msec --picosecondsToDiffTime (msec * )
          (year, b2)   = case timeType of
                             TimeUTC         -> first ((1900 +) . centurize . toInt) $ splitAt 2 b
                             TimeGeneralized -> first toInt $ splitAt 4 b
          (month, b3)  = first toInt $ splitAt 2 b2
          (day, b4)    = first toInt $ splitAt 2 b3
          (hour, b5)   = first toInt $ splitAt 2 b4
          (minute, b6) = first toInt $ splitAt 2 b5
          (sec, b7)    = first toInt $ splitAt 2 b6
          (msec, b8)   = case b7 of -- parse .[0-9]
                            0x2e:b7' -> first toPico $ spanToLength 3 (\c -> fromIntegral c >= ord '0' && fromIntegral c <= ord '9') b7'
                            _        -> (0,b7)
          (tz, _)      = case b8 of
                            0x5a:b8' -> (Just utc, b8') -- zulu
                            0x2b:b8' -> (Just undefined, b8') -- +
                            0x2d:b8' -> (Just undefined, b8') -- -
                            _        -> (Nothing, b8)

          spanToLength :: Int -> (Word8 -> Bool) -> [Word8] -> ([Word8], [Word8])
          spanToLength len p l = loop 0 l
            where loop i z
                     | i >= len  = ([], z)
                     | otherwise = case z of
                                        []   -> ([], [])
                                        x:xs -> if p x
                                                    then let (r1,r2) = loop (i+1) xs
                                                          in (x:r1, r2)
                                                    else ([], z)

          toPico :: [Word8] -> Integer
          toPico l = toInt l * order * 1000000000
            where len   = length l
                  order = case len of
                            1 -> 100
                            2 -> 10
                            3 -> 1
                            _ -> 1

          toInt :: [Word8] -> Integer
          toInt         = foldl (\acc w -> acc * 10 + fromIntegral (fromIntegral w - ord '0')) 0

          centurize v
            | v <= 50   = v + 100
            | otherwise = v

putTime :: ASN1TimeType -> UTCTime -> Maybe TimeZone  -> ByteString
putTime ty (UTCTime day diff) mtz = B.pack etime
    where
        etime
            | ty == TimeUTC = [y3, y4, m1, m2, d1, d2, h1, h2, mi1, mi2, s1, s2]++tzStr
            | otherwise     = [y1, y2, y3, y4, m1, m2, d1, d2, h1, h2, mi1, mi2, s1, s2]++msecStr++tzStr

        charZ = 90

        msecStr = []
        tzStr = case mtz of
                     Nothing                           -> []
                     Just tz | timeZoneMinutes tz == 0 -> [charZ]
                             | otherwise               -> asciiToWord8 $ timeZoneOffsetString tz

        (y_,m,d) = toGregorian day
        y        = fromIntegral y_

        secs     = truncate (realToFrac diff :: Double) :: Integer

        (h,mins) = secs `divMod` 3600
        (mi,s)   = mins `divMod` 60

        split2 n          = (fromIntegral $ n `div` 10 + ord '0', fromIntegral $ n `mod` 10 + ord '0')
        ((y1,y2),(y3,y4)) = (split2 (y `div` 100), split2 (y `mod` 100))
        (m1, m2)          = split2 m
        (d1, d2)          = split2 d
        (h1, h2)          = split2 $ fromIntegral h
        (mi1, mi2)        = split2 $ fromIntegral mi
        (s1, s2)          = split2 $ fromIntegral s

        asciiToWord8 :: [Char] -> [Word8]
        asciiToWord8 = map (fromIntegral . fromEnum)

putInteger :: Integer -> ByteString
putInteger i = B.pack $ bytesOfInt i

putBitString :: BitArray -> ByteString
putBitString (BitArray n bits) =
	B.concat [B.singleton (fromIntegral i),bits]
	where i = (8 - (n `mod` 8)) .&. 0x7

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
