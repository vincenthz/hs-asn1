-- |
-- Module      : Data.ASN1.CER
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing ASN1 CER specification serialization/derialization tools
--
module Data.ASN1.CER
	( ASN1Class(..)
	, ASN1(..)

	-- * CER serial functions
	, decodeASN1s
	, encodeASN1s
	, decodeASN1
	, encodeASN1
	) where

import Data.ASN1.Event
import Data.ASN1.Prim
import Data.ASN1.Types (ASN1t)
import qualified Data.ASN1.BER as BER
import qualified Data.ByteString.Lazy as L

{-
- 9.2        String encoding forms
string values shall be encoded with a primitive encoding if they would
require no more than 1000 contents octets, and as a constructed encoding otherwise.
The string fragments contained in the constructed encoding shall be encoded with a primitive encoding.
The encoding of each fragment, except possibly the last, shall have 1000 contents octets.
-}
{-
putStringCER :: Int -> L.ByteString -> ValStruct
putStringCER tn l =
	if L.length l > 1000
		then Constructed $ map (Value Universal tn . (Primitive . B.concat . L.toChunks)) $ repack1000 l
		else Primitive $ B.concat $ L.toChunks l
	where
		repack1000 x =
			if L.length x > 1000
				then 
					let (x1, x2) = L.splitAt 1000 x in
					x1 : repack1000 x2
				else
					[ x ]
-}

{-# DEPRECATED decodeASN1s "use stream types with decodeASN1Stream" #-}
decodeASN1s :: L.ByteString -> Either ASN1Err [ASN1t]
decodeASN1s = BER.decodeASN1s

{-# DEPRECATED decodeASN1 "use stream types with decodeASN1Stream" #-}
decodeASN1 :: L.ByteString -> Either ASN1Err ASN1t
decodeASN1 = BER.decodeASN1

{-# DEPRECATED encodeASN1s "use stream types with encodeASN1Stream" #-}
encodeASN1s :: [ASN1t] -> L.ByteString
encodeASN1s = BER.encodeASN1s

{-# DEPRECATED encodeASN1 "use stream types with encodeASN1Stream" #-}
encodeASN1 :: ASN1t -> L.ByteString
encodeASN1 = BER.encodeASN1
