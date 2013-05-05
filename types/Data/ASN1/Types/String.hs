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

-- T61 encoding : http://www.mail-archive.com/asn1@asn1.org/msg00460.html

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
  where asciiLikeEncodings = [IA5,Numeric,Printable,Visible,General,Graphic]

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
decodeUTF8 b
    | B.null b  = []
    | otherwise =
        case B.index b 0 of
            v | v `isClear` 7 -> toEnum (fromIntegral v) : decodeUTF8 (B.drop 1 b)
              | v `isClear` 6 -> error "continuation byte"
              | v `isClear` 5 -> uncont 1 (v .&. 0x1f)
              | v `isClear` 4 -> uncont 2 (v .&. 0xf)
              | v `isClear` 3 -> uncont 3 (v .&. 0x7)
              | otherwise     -> error "too many byte"
  where uncont n iniV
            | B.length b < (n+1) = error "cannot decode"
            | otherwise          = let z = flip map [1..n] $ \i ->
                                        case B.index b (i+1) of
                                            v | v `testBit` 7 && v `isClear` 6 -> v .&. 0x3f
                                              | otherwise                      -> error "not a continuation byte"
                                    in (toEnum $ fromIntegral $ foldl (\acc v -> (acc `shiftL` 6) + v) iniV z) : decodeUTF8 (B.drop (1+n) b)
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
        toCont v = fromIntegral (0xc0 .&. (v .&. 0x3f))

decodeASCII :: ByteString -> String
decodeASCII = BC.unpack

encodeASCII :: String -> ByteString
encodeASCII = BC.pack

decodeBMP :: ByteString -> String
decodeBMP b
    | odd (B.length b) = error "not a valid BMP string"
    | otherwise        = undefined
encodeBMP :: String -> ByteString
encodeBMP s = B.pack $ concatMap (toUCS2 . fromEnum) s
  where toUCS2 v = [b0,b1]
            where b0 = fromIntegral (v `shiftR` 8)
                  b1 = fromIntegral (v .&. 0xff)

decodeUTF32 :: ByteString -> String
decodeUTF32 b
    | (B.length b `mod` 4) /= 0 = error "not a valid UTF32 string"
    | otherwise                 = undefined
encodeUTF32 :: String -> ByteString
encodeUTF32 s = B.pack $ concatMap (toUTF32 . fromEnum) s
  where toUTF32 v = [b0,b1,b2,b3]
            where b0 = fromIntegral (v `shiftR` 24)
                  b1 = fromIntegral ((v `shiftR` 16) .&. 0xff)
                  b2 = fromIntegral ((v `shiftR` 8)  .&. 0xff)
                  b3 = fromIntegral (v .&. 0xff)
