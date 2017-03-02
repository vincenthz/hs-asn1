-- |
-- Module      : Data.ASN1.Error
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module Data.ASN1.Error
    (
    -- * Errors types
      ASN1Error(..)
    ) where

import Control.Exception (Exception)
import Data.Typeable

-- | Possible errors during parsing operations
data ASN1Error = StreamUnexpectedEOC         -- ^ Unexpected EOC in the stream.
               | StreamInfinitePrimitive     -- ^ Invalid primitive with infinite length in a stream.
               | StreamConstructionWrongSize -- ^ A construction goes over the size specified in the header.
               | StreamUnexpectedSituation String -- ^ An unexpected situation has come up parsing an ASN1 event stream.
               | ParsingHeaderFail String    -- ^ Parsing an invalid header.
               | ParsingPartial              -- ^ Parsing is not finished, there is construction unended.
               | TypeNotImplemented String   -- ^ Decoding of a type that is not implemented. Contribution welcome.
               | TypeDecodingFailed String   -- ^ Decoding of a knowed type failed.
               | TypePrimitiveInvalid String -- ^ Invalid primitive type
               | PolicyFailed String String -- ^ Policy failed including the name of the policy and the reason.
               deriving (Typeable, Show, Eq)

instance Exception ASN1Error
