module Data.ASN1.Internal
	( uintOfBytes
	, intOfBytes
	, bytesOfUInt
	, bytesOfInt
        , asn1SevenBit
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
bytesOfUInt x = reverse (list x)
	where
		list i = if i <= 0xff then [fromIntegral i] else (fromIntegral i .&. 0xff) : list (i `shiftR` 8)

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
	| i > 0      = if testBit (head uints) 7 then 0 : uints else uints
	| i == 0     = [0]
	| otherwise  = if testBit (head nints) 7 then nints else 0xff : nints
	where
		uints = bytesOfUInt (abs i)
		nints = reverse $ plusOne $ reverse $ map complement $ uints
		plusOne []     = [1]
		plusOne (x:xs) = if x == 0xff then 0 : plusOne xs else (x+1) : xs

{- | 

ASN1 often uses a particular kind of 7-bit encoding of integers like
in the case of long tags or encoding of integer component of OID's.
Use this function for such an encoding. Assumes a positive integer.

-}

asn1SevenBit :: (Bits i, Integral i) => i -> ByteString
asn1SevenBit i = B.reverse . turnUpBits $ B.unfoldr unf i
    where unf x | x == 0 = Nothing
                | x >  0 = let out  = x .&. 0x7F
                               rest = shiftR x 7
                           in Just (fromIntegral out, rest)
          turnUpBits = snd . B.mapAccumL mp 0
                     where mp a w = (0x80, w .|. a)

{- 

   Here is the description of the algorithm of the above encoding: 

1. The integer is chunked up into 7-bit groups. Each of these 7bit
   chunks are encoded as a single octect.

2. All the octects except the last one has its 8th bit set.

3. The function turnUpBits does this turning up of bits. Note that
   after the unfoldr the number is in reverse order hence we use a
   mapAccumL.  The first byte will not have its bit turned on but all
   others will.

-}
