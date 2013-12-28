-- |
-- Module      : Data.ASN1.BitArray
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE DeriveDataTypeable #-}
module Data.ASN1.BitArray
    ( BitArray(..)
    , BitArrayOutOfBound(..)
    , bitArrayLength
    , bitArrayGetBit
    , bitArraySetBitValue
    , bitArraySetBit
    , bitArrayClearBit
    , bitArrayGetData
    , toBitArray
    ) where

import Data.Bits
import Data.Word
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Typeable
import Control.Exception (Exception, throw)

-- | throwed in case of out of bounds in the bitarray.
data BitArrayOutOfBound = BitArrayOutOfBound Word64
    deriving (Show,Eq,Typeable)
instance Exception BitArrayOutOfBound

-- | represent a bitarray / bitmap
--
-- the memory representation start at bit 0
data BitArray = BitArray Word64 ByteString
    deriving (Show,Eq)

-- | returns the length of bits in this bitarray
bitArrayLength :: BitArray -> Word64
bitArrayLength (BitArray l _) = l

bitArrayOutOfBound :: Word64 -> a
bitArrayOutOfBound n = throw $ BitArrayOutOfBound n

-- | get the nth bits
bitArrayGetBit :: BitArray -> Word64 -> Bool
bitArrayGetBit (BitArray l d) n
    | n >= l    = bitArrayOutOfBound n
    | otherwise = flip testBit (7-fromIntegral bitn) $ B.index d (fromIntegral offset)
        where (offset, bitn) = n `divMod` 8

-- | set the nth bit to the value specified
bitArraySetBitValue :: BitArray -> Word64 -> Bool -> BitArray
bitArraySetBitValue (BitArray l d) n v
    | n >= l    = bitArrayOutOfBound n
    | otherwise =
        let (before,after) = B.splitAt (fromIntegral offset) d in
        -- array bound check before prevent fromJust from failing.
        let (w,remaining) = fromJust $ B.uncons after in
        BitArray l (before `B.append` (setter w (7-fromIntegral bitn) `B.cons` remaining))
  where
        (offset, bitn) = n `divMod` 8
        setter = if v then setBit else clearBit

-- | set the nth bits
bitArraySetBit :: BitArray -> Word64 -> BitArray
bitArraySetBit bitarray n = bitArraySetBitValue bitarray n True

-- | clear the nth bits
bitArrayClearBit :: BitArray -> Word64 -> BitArray
bitArrayClearBit bitarray n = bitArraySetBitValue bitarray n False

-- | get padded bytestring of the bitarray
bitArrayGetData :: BitArray -> ByteString
bitArrayGetData (BitArray _ d) = d

-- | number of bit to skip at the end (padding)
toBitArray :: ByteString -> Int -> BitArray
toBitArray l toSkip =
    BitArray (fromIntegral (B.length l * 8 - fromIntegral toSkip)) l
