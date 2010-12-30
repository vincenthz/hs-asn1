-- |
-- Module      : Data.ASN1.DER
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing ASN1 DER specification serialization/derialization tools
--
module Data.ASN1.DER
	( ASN1Class(..)
	, ASN1(..)

	-- * DER serialize functions
	{-
	, decodeASN1Get
	, decodeASN1State
	-}
	, decodeASN1
	, decodeASN1s
	, encodeASN1
	, encodeASN1s
	) where

import Data.ASN1.Raw
import Data.ASN1.Prim
import Data.ASN1.Types (ASN1t)
import qualified Data.ASN1.BER as BER
import qualified Data.ByteString.Lazy as L

{-

{- | Check if the length is the minimum possible and it's not indefinite -}
checkLength :: ASN1Length -> Maybe ASN1Err
checkLength LenIndefinite = Just $ ASN1PolicyFailed "DER" "indefinite length not allowed"
checkLength (LenShort _)  = Nothing
checkLength (LenLong n i)
	| n == 1 && i < 0x80  = Just $ ASN1PolicyFailed "DER" "long length should be a short length"
	| n == 1 && i >= 0x80 = Nothing
	| otherwise           = if i >= 2^((n-1)*8) && i < 2^(n*8) then Nothing else Just $ ASN1PolicyFailed "DER" "long length is not shortest"

{- | check if the value type is correct -}
checkType :: ASN1Class -> ASN1Tag -> Maybe ASN1Err
checkType _ _ = Nothing

{- | check if the value is bounded by DER policies -}
check :: (ASN1Class, Bool, ASN1Tag) -> ASN1Length -> Maybe ASN1Err
check (tc,_,tn) vallen = checkLength vallen `mplus` checkType tc tn

{- | ofRaw same as BER.ofRAW but check some additional DER constraint. -}
ofRaw :: Value -> Either ASN1Err ASN1
ofRaw (Value Universal 0x1 (Primitive b)) = getBoolean True b
ofRaw v                                   = BER.ofRaw v

{- | toRaw create a DER encoded value ready -}
toRaw :: ASN1 -> Value
toRaw = BER.toRaw

decodeASN1Get :: Get (Either ASN1Err ASN1)
decodeASN1Get = runGetErrInGet (getValueCheck check) >>= return . either Left ofRaw

decodeASN1State :: L.ByteString -> Either ASN1Err (ASN1, L.ByteString, Int64)
decodeASN1State b =
	runGetErrState (getValueCheck check >>= either throwError return . BER.ofRaw) b 0

decodeASN1 :: L.ByteString -> Either ASN1Err ASN1
decodeASN1 b = either Left BER.ofRaw $ runGetErr (getValueCheck check) b

decodeASN1s :: L.ByteString -> Either ASN1Err [ASN1]
decodeASN1s = loop where
	loop z = case decodeASN1State z of
		Left err -> throwError err
		Right (v, rest, _) -> if L.length rest > 0 then liftM (v :) (loop rest) else return [v]
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
