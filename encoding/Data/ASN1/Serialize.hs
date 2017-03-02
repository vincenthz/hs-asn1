-- |
-- Module      : Data.ASN1.Serialize
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ASN1.Serialize (getHeader, putHeader) where

import qualified Data.ByteString as B
import Data.ASN1.Get
import Data.ASN1.Internal
import Data.ASN1.Types
import Data.ASN1.Types.Lowlevel
import Data.Bits
import Data.Word
import Control.Applicative ((<$>))
import Control.Monad

-- | parse an ASN1 header
getHeader :: Get ASN1Header
getHeader = do
    (cl,pc,t1) <- parseFirstWord <$> getWord8
    tag        <- if t1 == 0x1f then getTagLong else return t1
    len        <- getLength
    return $ ASN1Header cl tag pc len

-- | Parse the first word of an header
parseFirstWord :: Word8 -> (ASN1Class, Bool, ASN1Tag)
parseFirstWord w = (cl,pc,t1)
  where cl = toEnum $ fromIntegral $ (w `shiftR` 6)
        pc = testBit w 5
        t1 = fromIntegral (w .&. 0x1f)

{- when the first tag is 0x1f, the tag is in long form, where
 - we get bytes while the 7th bit is set. -}
getTagLong :: Get ASN1Tag
getTagLong = do
    t <- fromIntegral <$> getWord8
    when (t == 0x80) $ fail "non canonical encoding of long tag"
    if testBit t 7
        then loop (clearBit t 7)
        else return t
  where loop n = do
            t <- fromIntegral <$> getWord8
            if testBit t 7
                then loop (n `shiftL` 7 + clearBit t 7)
                else return (n `shiftL` 7 + t)


{- get the asn1 length which is either short form if 7th bit is not set,
 - indefinite form is the 7 bit is set and every other bits clear,
 - or long form otherwise, where the next bytes will represent the length
 -}
getLength :: Get ASN1Length
getLength = do
    l1 <- fromIntegral <$> getWord8
    if testBit l1 7
        then case clearBit l1 7 of
            0   -> return LenIndefinite
            len -> do
                lw <- getBytes len
                return (LenLong len $ uintbs lw)
        else
            return (LenShort l1)
  where
        {- uintbs return the unsigned int represented by the bytes -}
        uintbs = B.foldl (\acc n -> (acc `shiftL` 8) + fromIntegral n) 0

-- | putIdentifier encode an ASN1 Identifier into a marshalled value
putHeader :: ASN1Header -> B.ByteString
putHeader (ASN1Header cl tag pc len) = B.concat
    [ B.singleton word1
    , if tag < 0x1f then B.empty else tagBS
    , lenBS]
  where cli   = shiftL (fromIntegral $ fromEnum cl) 6
        pcval = shiftL (if pc then 0x1 else 0x0) 5
        tag0  = if tag < 0x1f then fromIntegral tag else 0x1f
        word1 = cli .|. pcval .|. tag0
        lenBS = B.pack $ putLength len
        tagBS = putVarEncodingIntegral tag

{- | putLength encode a length into a ASN1 length.
 - see getLength for the encoding rules -}
putLength :: ASN1Length -> [Word8]
putLength (LenShort i)
    | i < 0 || i > 0x7f = error "putLength: short length is not between 0x0 and 0x80"
    | otherwise         = [fromIntegral i]
putLength (LenLong _ i)
    | i < 0     = error "putLength: long length is negative"
    | otherwise = lenbytes : lw
        where
            lw       = bytesOfUInt $ fromIntegral i
            lenbytes = fromIntegral (length lw .|. 0x80)
putLength (LenIndefinite) = [0x80]
