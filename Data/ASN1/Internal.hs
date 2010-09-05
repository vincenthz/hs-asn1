module Data.ASN1.Internal (
	uintOfBytes,
	intOfBytes,
	bytesOfUInt,
	bytesOfInt
	) where

import Data.Word
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B

{- | uintOfBytes returns the number of bytes and the unsigned integer represented by the bytes -}
uintOfBytes :: ByteString -> (Int, Integer)
uintOfBytes b = (B.length b, B.foldl (\acc n -> (acc `shiftL` 8) + fromIntegral n) 0 b)

--bytesOfUInt i = B.unfoldr (\x -> if x == 0 then Nothing else Just (fromIntegral (x .&. 0xff), x `shiftR` 8)) i
bytesOfUInt :: Integer -> [Word8]
bytesOfUInt i = if i <= 0xff then [fromIntegral i] else (fromIntegral i .&. 0xff) : bytesOfUInt (i `shiftR` 8)

{- | intOfBytes returns the number of bytes in the list and
   the represented integer by a two's completement list of bytes -}
intOfBytes :: ByteString -> (Int, Integer)
intOfBytes b
	| B.length b == 0   = (0, 0)
	| otherwise         = (len, if isNeg then -(maxIntLen - v + 1) else v)
	where
		(len, v)  = uintOfBytes b
		maxIntLen = 2 ^ (8 * len) - 1
		isNeg     = testBit (B.head b) 7

{- | bytesOfInt convert an integer into a two's completemented list of bytes -}
bytesOfInt :: Integer -> [Word8]
bytesOfInt i
	| i > 0      = ints
	| i == 0     = [0]
	| otherwise  = reverse $ plusOne $ reverse $ map complement $ ints
	where
		uints = bytesOfUInt (abs i)
		isNeg = testBit (head uints) 7
		ints  = if isNeg then 0 : uints else uints
		plusOne []     = []
		plusOne (x:xs) = if x == 0xff then 0 : plusOne xs else (x+1) : xs
