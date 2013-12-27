-- |
-- Module      : Data.ASN1.Encoding
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ASN1.Encoding
    (
    -- * generic class for decoding and encoding stream
      ASN1Decoding(..)
    , ASN1DecodingRepr(..)
    , ASN1Encoding(..)
    -- * strict bytestring version
    , decodeASN1'
    , decodeASN1Repr'
    , encodeASN1'
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ASN1.Stream
import Data.ASN1.Types
import Data.ASN1.Error

-- | Describe an ASN1 decoding, that transform a bytestream into an asn1stream
class ASN1Decoding a where
    -- | decode a lazy bytestring into an ASN1 stream
    decodeASN1 :: a -> L.ByteString -> Either ASN1Error [ASN1]

-- | transition class.
class ASN1DecodingRepr a where
    -- | decode a lazy bytestring into an ASN1 stream
    decodeASN1Repr :: a -> L.ByteString -> Either ASN1Error [ASN1Repr]

-- | Describe an ASN1 encoding, that transform an asn1stream into a bytestream
class ASN1Encoding a where
    -- | encode a stream into a lazy bytestring
    encodeASN1 :: a -> [ASN1] -> L.ByteString

-- | decode a strict bytestring into an ASN1 stream
decodeASN1' :: ASN1Decoding a => a -> B.ByteString -> Either ASN1Error [ASN1]
decodeASN1' encoding bs = decodeASN1 encoding $ L.fromChunks [bs]

-- | decode a strict bytestring into an ASN1Repr stream
decodeASN1Repr' :: ASN1DecodingRepr a => a -> B.ByteString -> Either ASN1Error [ASN1Repr]
decodeASN1Repr' encoding bs = decodeASN1Repr encoding $ L.fromChunks [bs]

-- | encode a stream into a strict bytestring
encodeASN1' :: ASN1Encoding a => a -> [ASN1] -> B.ByteString
encodeASN1' encoding = B.concat . L.toChunks . encodeASN1 encoding
