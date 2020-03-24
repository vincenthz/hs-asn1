-- |
-- Module      : Data.ASN1.Types.String
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Different String types available in ASN1
--
module Data.ASN1.Types.String
    ( ASN1StringEncoding(..)
    , ASN1CharacterString(..)
    , asn1CharacterString
    , asn1CharacterToString
    ) where

import Data.String
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Bits
import Data.Word

-- a note on T61 encodings. The actual specification of a T61 character set seems
-- to be lost in time, as such it will be considered an ascii like encoding.
--
-- <http://www.mail-archive.com/asn1@asn1.org/msg00460.html>
-- "sizable volume of software in the world treats TeletexString (T61String)
-- as a simple 8-bit string with mostly Windows Latin 1"

-- | Define all possible ASN1 String encoding.
data ASN1StringEncoding =
      IA5       -- ^ 128 characters equivalent to the ASCII alphabet
    | UTF8      -- ^ UTF8
    | General   -- ^ all registered graphic and character sets (see ISO 2375) plus SPACE and DELETE.
    | Graphic   -- ^ all registered G sets and SPACE
    | Numeric   -- ^ encoding containing numeric [0-9] and space
    | Printable -- ^ printable [a-z] [A-Z] [()+,-.?:/=] and space.
    | VideoTex  -- ^ CCITT's T.100 and T.101 character sets
    | Visible   -- ^ International ASCII printing character sets
    | T61       -- ^ teletext
    | UTF32     -- ^ UTF32
    | Character -- ^ Character
    | BMP       -- ^ UCS2
    deriving (Show,Eq,Ord)

-- | provide a way to possibly encode or decode character string based on character encoding
stringEncodingFunctions :: ASN1StringEncoding
                        -> Maybe (ByteString -> String, String -> ByteString)
stringEncodingFunctions encoding
    | encoding == UTF8                   = Just (decodeUTF8, encodeUTF8)
    | encoding == BMP                    = Just (decodeBMP, encodeBMP)
    | encoding == UTF32                  = Just (decodeUTF32, encodeUTF32)
    | encoding `elem` asciiLikeEncodings = Just (decodeASCII, encodeASCII)
    | otherwise                          = Nothing
  where asciiLikeEncodings = [IA5,Numeric,Printable,Visible,General,Graphic,T61]

-- | encode a string into a character string
asn1CharacterString :: ASN1StringEncoding -> String -> ASN1CharacterString
asn1CharacterString encoding s =
    case stringEncodingFunctions encoding of
        Just (_, e) -> ASN1CharacterString encoding (e s)
        Nothing     -> error ("cannot encode ASN1 Character String " ++ show encoding ++ " from string")

-- | try to decode an 'ASN1CharacterString' to a String
asn1CharacterToString :: ASN1CharacterString -> Maybe String
asn1CharacterToString (ASN1CharacterString encoding bs) =
    case stringEncodingFunctions encoding of
        Just (d, _) -> Just (d bs)
        Nothing     -> Nothing

-- | ASN1 Character String with encoding
data ASN1CharacterString = ASN1CharacterString
    { characterEncoding         :: ASN1StringEncoding
    , getCharacterStringRawData :: ByteString
    } deriving (Show,Eq,Ord)

instance IsString ASN1CharacterString where
    fromString s = ASN1CharacterString UTF8 (encodeUTF8 s)

decodeUTF8 :: ByteString -> String
decodeUTF8 b = loop 0 $ B.unpack b
  where loop :: Int -> [Word8] -> [Char]
        loop _   []     = []
        loop pos (x:xs)
            | x `isClear` 7 = toEnum (fromIntegral x) : loop (pos+1) xs
            | x `isClear` 6 = error "continuation byte in heading context"
            | x `isClear` 5 = uncont 1 (x .&. 0x1f) pos xs
            | x `isClear` 4 = uncont 2 (x .&. 0xf)  pos xs
            | x `isClear` 3 = uncont 3 (x .&. 0x7)  pos xs
            | otherwise     = error "too many byte"
        uncont :: Int -> Word8 -> Int -> [Word8] -> [Char]
        uncont 1 iniV pos xs =
            case xs of
                c1:xs' -> decodeCont iniV [c1] : loop (pos+2) xs'
                _      -> error "truncated continuation, expecting 1 byte"
        uncont 2 iniV pos xs =
            case xs of
                c1:c2:xs' -> decodeCont iniV [c1,c2] : loop (pos+3) xs'
                _         -> error "truncated continuation, expecting 2 bytes"
        uncont 3 iniV pos xs =
            case xs of
                c1:c2:c3:xs' -> decodeCont iniV [c1,c2,c3] : loop (pos+4) xs'
                _            -> error "truncated continuation, expecting 3 bytes"
        uncont _ _ _ _ = error "invalid number of bytes for continuation"
        decodeCont :: Word8 -> [Word8] -> Char
        decodeCont iniV l
            | all isContByte l = toEnum $ foldl (\acc v -> (acc `shiftL` 6) + fromIntegral v) (fromIntegral iniV) $ map (\v -> v .&. 0x3f) l
            | otherwise        = error "continuation bytes invalid"
        isContByte v = v `testBit` 7 && v `isClear` 6
        isClear v i = not (v `testBit` i)

encodeUTF8 :: String -> ByteString
encodeUTF8 s = B.pack $ concatMap (toUTF8 . fromEnum) s
  where toUTF8 e
            | e < 0x80      = [fromIntegral e]
            | e < 0x800     = [fromIntegral (0xc0 .|. (e `shiftR` 6)), toCont e]
            | e < 0x10000   = [fromIntegral (0xe0 .|. (e `shiftR` 12))
                              ,toCont (e `shiftR` 6)
                              ,toCont e]
            | e < 0x200000  = [fromIntegral (0xf0 .|. (e `shiftR` 18))
                              , toCont (e `shiftR` 12)
                              , toCont (e `shiftR` 6)
                              , toCont e]
            | otherwise     = error "not a valid value"
        toCont v = fromIntegral (0x80 .|. (v .&. 0x3f))

decodeASCII :: ByteString -> String
decodeASCII = BC.unpack

encodeASCII :: String -> ByteString
encodeASCII = BC.pack

decodeBMP :: ByteString -> String
decodeBMP b
    | odd (B.length b) = error "not a valid BMP string"
    | otherwise        = fromUCS2 $ B.unpack b
  where fromUCS2 [] = []
        fromUCS2 (b0:b1:l) =
            let v :: Word16
                v = (fromIntegral b0 `shiftL` 8) .|. fromIntegral b1
             in toEnum (fromIntegral v) : fromUCS2 l
        fromUCS2 _ = error "decodeBMP: internal error"
encodeBMP :: String -> ByteString
encodeBMP s = B.pack $ concatMap (toUCS2 . fromEnum) s
  where toUCS2 v = [b0,b1]
            where b0 = fromIntegral (v `shiftR` 8)
                  b1 = fromIntegral (v .&. 0xff)

decodeUTF32 :: ByteString -> String
decodeUTF32 bs
    | (B.length bs `mod` 4) /= 0 = error "not a valid UTF32 string"
    | otherwise                  = fromUTF32 0
  where w32ToChar :: Word32 -> Char
        w32ToChar = toEnum . fromIntegral
        fromUTF32 ofs
            | ofs == B.length bs = []
            | otherwise =
                let a = B.index bs ofs
                    b = B.index bs (ofs+1)
                    c = B.index bs (ofs+2)
                    d = B.index bs (ofs+3)
                    v = (fromIntegral a `shiftL` 24) .|.
                        (fromIntegral b `shiftL` 16) .|.
                        (fromIntegral c `shiftL` 8) .|.
                        (fromIntegral d)
                 in w32ToChar v : fromUTF32 (ofs+4)
encodeUTF32 :: String -> ByteString
encodeUTF32 s = B.pack $ concatMap (toUTF32 . fromEnum) s
  where toUTF32 v = [b0,b1,b2,b3]
            where b0 = fromIntegral (v `shiftR` 24)
                  b1 = fromIntegral ((v `shiftR` 16) .&. 0xff)
                  b2 = fromIntegral ((v `shiftR` 8)  .&. 0xff)
                  b3 = fromIntegral (v .&. 0xff)
