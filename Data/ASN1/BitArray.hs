{-# LANGUAGE DeriveDataTypeable #-}
module Data.ASN1.BitArray
	( BitArray(..)
	, BitArrayOutOfBound(..)
	, bitArrayLength
	, bitArrayGetBit
	, bitArrayGetData
	, toBitArray
	) where

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as L
import Data.Typeable
import Control.Exception (Exception, throw)

-- | throwed in case of out of bounds in the bitarray.
data BitArrayOutOfBound = BitArrayOutOfBound Word64
	deriving (Show,Eq,Typeable)
instance Exception BitArrayOutOfBound

-- | represent a bitarray / bitmap
data BitArray = BitArray Word64 L.ByteString
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
	| otherwise = flip testBit (7-fromIntegral bitn) $ L.index d (fromIntegral offset)
		where (offset, bitn) = n `divMod` 8

-- | get padded bytestring of the bitarray
bitArrayGetData :: BitArray -> L.ByteString
bitArrayGetData (BitArray _ d) = d

-- | number of bit to skip at the end (padding)
toBitArray :: L.ByteString -> Int -> BitArray
toBitArray l toSkip =
	BitArray (fromIntegral (L.length l * 8 - fromIntegral toSkip)) l
