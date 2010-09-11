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
	( TagClass(..)
	, ASN1(..)

	-- * DER serialize functions
	, decodeASN1Get
	, decodeASN1
	, encodeASN1Put
	, encodeASN1
	) where

import Data.ASN1.Raw
import Data.ASN1.Prim
import Data.Binary.Get
import Data.Binary.Put
import Control.Monad (mplus)
import qualified Data.ASN1.BER as BER
import qualified Data.ByteString.Lazy as L

{- | Check if the length is the minimum possible and it's not indefinite -}
checkLength :: ValLength -> Maybe ASN1Err
checkLength LenIndefinite = Just $ ASN1PolicyFailed "DER" "indefinite length not allowed"
checkLength (LenShort _)  = Nothing
checkLength (LenLong n i)
	| n == 1 && i < 0x80  = Just $ ASN1PolicyFailed "DER" "long length should be a short length"
	| n == 1 && i >= 0x80 = Nothing
	| otherwise           = if i >= 2^((n-1)*8) && i < 2^(n*8) then Nothing else Just $ ASN1PolicyFailed "DER" "long length is not shortest"

{- | check if the value type is correct -}
checkType :: TagClass -> TagNumber -> Maybe ASN1Err
checkType _ _ = Nothing

{- | check if the value is bounded by DER policies -}
check :: (TagClass, Bool, TagNumber) -> ValLength -> Maybe ASN1Err
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

decodeASN1 :: L.ByteString -> Either ASN1Err ASN1
decodeASN1 b = either Left BER.ofRaw $ runGetErr (getValueCheck check) b

encodeASN1Put :: ASN1 -> Put
encodeASN1Put d = putValue $ toRaw d

encodeASN1 :: ASN1 -> L.ByteString
encodeASN1 = runPut . encodeASN1Put
