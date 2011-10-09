module Data.ASN1.BitArray
	( BitArray(..)
	, bitArrayLength
	, bitArrayGetBit
	, bitArrayGetData
	, toBitArray
	) where

import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as L

-- | represent a bitarray / bitmap
data BitArray = BitArray Word64 L.ByteString
	deriving (Show,Eq)

-- | returns the length of bits in this bitarray
bitArrayLength :: BitArray -> Word64
bitArrayLength (BitArray l _) = l

-- | get the nth bits
bitArrayGetBit :: BitArray -> Word64 -> Bool
bitArrayGetBit (BitArray l d) n
	| n >= l    = error ("array bit out of bounds: requesting bit " ++ show n ++ " but only got " ++ show l ++ " bits")
	| otherwise = flip testBit (7-fromIntegral bitn) $ L.index d (fromIntegral offset)
		where (offset, bitn) = n `divMod` 8

-- | get padded bytestring of the bitarray
bitArrayGetData :: BitArray -> L.ByteString
bitArrayGetData (BitArray _ d) = d

-- | number of bit to skip at the end (padding)
toBitArray :: L.ByteString -> Int -> BitArray
toBitArray l toSkip =
	BitArray (fromIntegral (L.length l * 8 - fromIntegral toSkip)) l
