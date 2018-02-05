-- |
-- Module      : Data.ASN1.Prim
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Tools to read ASN1 primitive (e.g. boolean, int)
--

{-# LANGUAGE CPP #-}
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
    , getDouble
    , getBitString
    , getOctetString
    , getNull
    , getOID
    , getTime

    -- * marshall an ASN1 type to a bytestring
    , putTime
    , putInteger
    , putDouble
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
import Data.Monoid
import Data.Word
import Data.List (unfoldr)
import Data.ByteString (ByteString)
import Data.Char (ord, isDigit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as B
import Data.Hourglass
import Control.Arrow (first)
import Control.Applicative
import Control.Monad

encodeHeader :: Bool -> ASN1Length -> ASN1 -> ASN1Header
encodeHeader pc len (Boolean _)                = ASN1Header Universal 0x1 pc len
encodeHeader pc len (IntVal _)                 = ASN1Header Universal 0x2 pc len
encodeHeader pc len (BitString _)              = ASN1Header Universal 0x3 pc len
encodeHeader pc len (OctetString _)            = ASN1Header Universal 0x4 pc len
encodeHeader pc len Null                       = ASN1Header Universal 0x5 pc len
encodeHeader pc len (OID _)                    = ASN1Header Universal 0x6 pc len
encodeHeader pc len (Real _)                   = ASN1Header Universal 0x9 pc len
encodeHeader pc len (Enumerated _)             = ASN1Header Universal 0xa pc len
encodeHeader pc len (ASN1String cs)            = ASN1Header Universal (characterStringType $ characterEncoding cs) pc len
  where characterStringType UTF8      = 0xc
        characterStringType Numeric   = 0x12
        characterStringType Printable = 0x13
        characterStringType T61       = 0x14
        characterStringType VideoTex  = 0x15
        characterStringType IA5       = 0x16
        characterStringType Graphic   = 0x19
        characterStringType Visible   = 0x1a
        characterStringType General   = 0x1b
        characterStringType UTF32     = 0x1c
        characterStringType Character = 0x1d
        characterStringType BMP       = 0x1e
encodeHeader pc len (ASN1Time TimeUTC _ _)     = ASN1Header Universal 0x17 pc len
encodeHeader pc len (ASN1Time TimeGeneralized _ _) = ASN1Header Universal 0x18 pc len
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
encodePrimitiveData (Real d)            = putDouble d
encodePrimitiveData (Enumerated i)      = putInteger $ fromIntegral i
encodePrimitiveData (ASN1String cs)     = getCharacterStringRawData cs
encodePrimitiveData (ASN1Time ty ti tz) = putTime ty ti tz
encodePrimitiveData (Other _ _ b)       = b
encodePrimitiveData o                   = error ("not a primitive " ++ show o)

encodePrimitive :: ASN1 -> (Int, [ASN1Event])
encodePrimitive a =
    let b = encodePrimitiveData a
        blen = B.length b
        len = makeLength blen
        hdr = encodePrimitiveHeader len a
     in (B.length (putHeader hdr) + blen, [Header hdr, Primitive b])
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
    let (ys, zs)    = getConstructedEnd 0 xs
        (llen, lev) = encodeList zs
        (len, ev)   = encodeConstructed t ys
     in (llen + len, ev ++ lev)

encodeList (x:xs)           =
    let (llen, lev) = encodeList xs
        (len, ev)   = encodeOne x
     in (llen + len, ev ++ lev)

encodeConstructed :: ASN1 -> [ASN1] -> (Int, [ASN1Event])
encodeConstructed c@(Start _) children =
    (tlen, Header h : ConstructionBegin : events ++ [ConstructionEnd])
  where (clen, events) = encodeList children
        len  = mkSmallestLength clen
        h    = encodeHeader True len c
        tlen = B.length (putHeader h) + clen

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
decodePrimitive (ASN1Header Universal 0x9 _ _) p   = getDouble p
decodePrimitive (ASN1Header Universal 0xa _ _) p   = getEnumerated p
decodePrimitive (ASN1Header Universal 0xb _ _) _   = Left $ TypeNotImplemented "EMBEDDED PDV"
decodePrimitive (ASN1Header Universal 0xc _ _) p   = getCharacterString UTF8 p
decodePrimitive (ASN1Header Universal 0xd _ _) _   = Left $ TypeNotImplemented "RELATIVE-OID"
decodePrimitive (ASN1Header Universal 0x10 _ _) _  = Left $ TypePrimitiveInvalid "sequence"
decodePrimitive (ASN1Header Universal 0x11 _ _) _  = Left $ TypePrimitiveInvalid "set"
decodePrimitive (ASN1Header Universal 0x12 _ _) p  = getCharacterString Numeric p
decodePrimitive (ASN1Header Universal 0x13 _ _) p  = getCharacterString Printable p
decodePrimitive (ASN1Header Universal 0x14 _ _) p  = getCharacterString T61 p
decodePrimitive (ASN1Header Universal 0x15 _ _) p  = getCharacterString VideoTex p
decodePrimitive (ASN1Header Universal 0x16 _ _) p  = getCharacterString IA5 p
decodePrimitive (ASN1Header Universal 0x17 _ _) p  = getTime TimeUTC p
decodePrimitive (ASN1Header Universal 0x18 _ _) p  = getTime TimeGeneralized p
decodePrimitive (ASN1Header Universal 0x19 _ _) p  = getCharacterString Graphic p
decodePrimitive (ASN1Header Universal 0x1a _ _) p  = getCharacterString Visible p
decodePrimitive (ASN1Header Universal 0x1b _ _) p  = getCharacterString General p
decodePrimitive (ASN1Header Universal 0x1c _ _) p  = getCharacterString UTF32 p
decodePrimitive (ASN1Header Universal 0x1d _ _) p  = getCharacterString Character p
decodePrimitive (ASN1Header Universal 0x1e _ _) p  = getCharacterString BMP p
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
{-# INLINE getInteger #-}
getInteger s = IntVal <$> getIntegerRaw "integer" s

{- | getEnumerated, parse an enumerated value the same way that integer values are parsed. -}
getEnumerated :: ByteString -> Either ASN1Error ASN1
{-# INLINE getEnumerated #-}
getEnumerated s = Enumerated <$> getIntegerRaw "enumerated" s

{- | According to X.690 section 8.4 integer and enumerated values should be encoded the same way. -}
getIntegerRaw :: String -> ByteString -> Either ASN1Error Integer
getIntegerRaw typestr s
    | B.length s == 0 = Left . TypeDecodingFailed $ typestr ++ ": null encoding"
    | B.length s == 1 = Right $ snd $ intOfBytes s
    | otherwise       =
        if (v1 == 0xff && testBit v2 7) || (v1 == 0x0 && (not $ testBit v2 7))
            then Left . TypeDecodingFailed $ typestr ++ ": not shortest encoding"
            else Right $ snd $ intOfBytes s
        where
            v1 = s `B.index` 0
            v2 = s `B.index` 1

getDouble :: ByteString -> Either ASN1Error ASN1
getDouble s = Real <$> getDoubleRaw s

getDoubleRaw :: ByteString -> Either ASN1Error Double
getDoubleRaw s
  | B.null s  = Right 0
getDoubleRaw s@(B.unsafeHead -> h)
  | h == 0x40 = Right $! (1/0)  -- Infinity
  | h == 0x41 = Right $! (-1/0) -- -Infinity
  | h == 0x42 = Right $! (0/0)  -- NaN
  | otherwise = do
      let len = B.length s
      base <- case (h `testBit` 5, h `testBit` 4) of
                -- extract bits 5,4 for the base
                (False, False) -> return 2
                (False, True)  -> return 8
                (True,  False) -> return 16
                _              -> Left . TypeDecodingFailed $ "real: invalid base detected"
      -- check bit 6 for the sign
      let mkSigned = if h `testBit` 6 then negate else id
      -- extract bits 3,2 for the scaling factor
      let scaleFactor = (h .&. 0x0c) `shiftR` 2
      expLength <- getExponentLength len h s
      -- 1 byte for the header, expLength for the exponent, and at least 1 byte for the mantissa
      unless (len > 1 + fromIntegral expLength) $
        Left . TypeDecodingFailed $ "real: not enough input for exponent and mantissa"
      let (_, exp'') = intOfBytes $ B.unsafeTake (fromIntegral expLength) $ B.unsafeDrop 1 s
      let exp' = case base :: Int of
                   2 -> exp''
                   8 -> 3 * exp''
                   _ -> 4 * exp'' -- must be 16
          exponent = exp' - fromIntegral scaleFactor
          -- whatever is leftover is the mantissa, unsigned
          (_, mantissa) = uintOfBytes $ B.unsafeDrop (1 + fromIntegral expLength) s
      Right $! encodeFloat (mkSigned $ toInteger mantissa) (fromIntegral exponent)

getExponentLength :: Int -> Word8 -> ByteString -> Either ASN1Error Word8
getExponentLength len h s =
  case h .&. 0x03 of
    l | l == 0x03 -> do
          unless (len > 1) $ Left . TypeDecodingFailed $ "real: not enough input to decode exponent length"
          return $ B.unsafeIndex s 1
      | otherwise -> return $ l + 1

getBitString :: ByteString -> Either ASN1Error ASN1
getBitString s =
    let toSkip = B.head s in
    let toSkip' = if toSkip >= 48 && toSkip <= 48 + 7 then toSkip - (fromIntegral $ ord '0') else toSkip in
    let xs = B.tail s in
    if toSkip' >= 0 && toSkip' <= 7
        then Right $ BitString $ toBitArray xs (fromIntegral toSkip')
        else Left $ TypeDecodingFailed ("bitstring: skip number not within bound " ++ show toSkip' ++ " " ++  show s)

getCharacterString :: ASN1StringEncoding -> ByteString -> Either ASN1Error ASN1
getCharacterString encoding bs = Right $ ASN1String (ASN1CharacterString encoding bs)

getOctetString :: ByteString -> Either ASN1Error ASN1
getOctetString = Right . OctetString

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
getTime timeType bs
    | hasNonASCII bs = decodingError "contains non ASCII characters"
    | otherwise      =
        case timeParseE format (BC.unpack bs) of -- BC.unpack is safe as we check ASCIIness first
            Left _  ->
                case timeParseE formatNoSeconds (BC.unpack bs) of
                    Left _  -> decodingError ("cannot convert string " ++ BC.unpack bs)
                    Right r -> parseRemaining r
            Right r -> parseRemaining r
  where
        parseRemaining r =
            case parseTimezone $ parseMs $ first adjustUTC r of
                Left err        -> decodingError err
                Right (dt', tz) -> Right $ ASN1Time timeType dt' tz

        adjustUTC dt@(DateTime (Date y m d) tod)
            | timeType == TimeGeneralized = dt
            | y > 2050                    = DateTime (Date (y - 100) m d) tod
            | otherwise                   = dt
        formatNoSeconds = init format
        format | timeType == TimeGeneralized = 'Y':'Y':baseFormat
               | otherwise                   = baseFormat
        baseFormat = "YYMMDDHMIS"

        parseMs (dt,s) =
            case s of
                '.':s' -> let (ns, r) = first toNano $ spanToLength 3 isDigit s'
                           in (dt { dtTime = (dtTime dt) { todNSec = ns } }, r)
                _      -> (dt,s)
        parseTimezone (dt,s) =
            case s of
                '+':s' -> Right (dt, parseTimezoneFormat id s')
                '-':s' -> Right (dt, parseTimezoneFormat ((-1) *) s')
                'Z':[] -> Right (dt, Just timezone_UTC)
                ""     -> Right (dt, Nothing)
                _      -> Left ("unknown timezone format: " ++ s)

        parseTimezoneFormat transform s
            | length s == 4  = Just $ toTz $ toInt $ fst $ spanToLength 4 isDigit s
            | otherwise      = Nothing
          where toTz z = let (h,m) = z `divMod` 100 in TimezoneOffset $ transform (h * 60 + m)

        toNano :: String -> NanoSeconds
        toNano l = fromIntegral (toInt l * order * 1000000)
          where len   = length l
                order = case len of
                            1 -> 100
                            2 -> 10
                            3 -> 1
                            _ -> 1

        spanToLength :: Int -> (Char -> Bool) -> String -> (String, String)
        spanToLength len p l = loop 0 l
          where loop i z
                    | i >= len  = ([], z)
                    | otherwise = case z of
                                    []   -> ([], [])
                                    x:xs -> if p x
                                                then let (r1,r2) = loop (i+1) xs
                                                      in (x:r1, r2)
                                                else ([], z)

        toInt :: String -> Int
        toInt = foldl (\acc w -> acc * 10 + (ord w - ord '0')) 0

        decodingError reason = Left $ TypeDecodingFailed ("time format invalid for " ++ show timeType ++ " : " ++ reason)
        hasNonASCII = maybe False (const True) . B.find (\c -> c > 0x7f)

-- FIXME need msec printed
putTime :: ASN1TimeType -> DateTime -> Maybe TimezoneOffset -> ByteString
putTime ty dt mtz = BC.pack etime
  where
        etime
            | ty == TimeUTC = timePrint "YYMMDDHMIS" dt ++ tzStr
            | otherwise     = timePrint "YYYYMMDDHMIS" dt ++ msecStr ++ tzStr
        msecStr = []
        tzStr = case mtz of
                     Nothing                      -> ""
                     Just tz | tz == timezone_UTC -> "Z"
                             | otherwise          -> show tz

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
putOID oids = case oids of
    (oid1:oid2:suboids) ->
        let eoidclass = fromIntegral (oid1 * 40 + oid2)
            subeoids  = B.concat $ map encode suboids
         in B.cons eoidclass subeoids
    _                   -> error ("invalid OID format " ++ show oids)
  where
        encode x | x == 0    = B.singleton 0
                 | otherwise = putVarEncodingIntegral x

putDouble :: Double -> ByteString
putDouble d
  | d == 0 = B.pack []
  | d == (1/0) = B.pack [0x40]
  | d == negate (1/0) = B.pack [0x41]
  | isNaN d = B.pack [0x42]
  | otherwise = B.cons (header .|. (expLen - 1)) -- encode length of exponent
                (expBS <> manBS)
  where
  (mkUnsigned, header)
    | d < 0     = (negate, bINARY_NEGATIVE_NUMBER_ID)
    | otherwise = (id, bINARY_POSITIVE_NUMBER_ID)
  (man, exp) = decodeFloat d
  (mantissa, exponent) = normalize (fromIntegral $ mkUnsigned man, exp)
  expBS = putInteger (fromIntegral exponent)
  expLen = fromIntegral (B.length expBS)
  manBS = putInteger (fromIntegral mantissa)

-- | Normalize the mantissa and adjust the exponent.
--
-- DER requires the mantissa to either be 0 or odd, so we right-shift it
-- until the LSB is 1, and then add the shift amount to the exponent.
--
-- TODO: handle denormal numbers
normalize :: (Word64, Int) -> (Word64, Int)
normalize (mantissa, exponent) = (mantissa `shiftR` sh, exponent + sh)
  where
    sh = countTrailingZeros mantissa

#if !(MIN_VERSION_base(4,8,0))
    countTrailingZeros :: FiniteBits b => b -> Int
    countTrailingZeros x = go 0
      where
        go i | i >= w      = i
             | testBit x i = i
             | otherwise   = go (i+1)
        w = finiteBitSize x
#endif

bINARY_POSITIVE_NUMBER_ID, bINARY_NEGATIVE_NUMBER_ID :: Word8
bINARY_POSITIVE_NUMBER_ID = 0x80
bINARY_NEGATIVE_NUMBER_ID = 0xc0
