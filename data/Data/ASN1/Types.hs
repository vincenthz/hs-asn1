-- |
-- Module      : Data.ASN1.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module Data.ASN1.Types
    (
    -- * Raw types
      ASN1Class(..)
    , ASN1Tag
    , ASN1Length(..)
    , ASN1Header(..)
    -- * Errors types
    , ASN1Error(..)
    -- * Events types
    , ASN1Event(..)
    ) where

import Control.Exception (Exception)
import Data.Typeable
import Data.ByteString (ByteString)

-- | Element class
data ASN1Class = Universal
               | Application
               | Context
               | Private
               deriving (Show,Eq,Ord,Enum)

-- | ASN1 Tag
type ASN1Tag = Int

-- | ASN1 Length with all different formats
data ASN1Length = LenShort Int      -- ^ Short form with only one byte. length has to be < 127.
                | LenLong Int Int   -- ^ Long form of N bytes
                | LenIndefinite     -- ^ Length is indefinite expect an EOC in the stream to finish the type
                deriving (Show,Eq)

-- | ASN1 Header with the class, tag, constructed flag and length.
data ASN1Header = ASN1Header !ASN1Class !ASN1Tag !Bool !ASN1Length
    deriving (Show,Eq)

-- | represent one event from an asn1 data stream
data ASN1Event = Header ASN1Header     -- ^ ASN1 Header
               | Primitive !ByteString -- ^ Primitive
               | ConstructionBegin     -- ^ Constructed value start
               | ConstructionEnd       -- ^ Constructed value end
               deriving (Show,Eq)

-- | Possible errors during parsing operations
data ASN1Error = StreamUnexpectedEOC         -- ^ Unexpected EOC in the stream.
               | StreamInfinitePrimitive     -- ^ Invalid primitive with infinite length in a stream.
               | StreamConstructionWrongSize -- ^ A construction goes over the size specified in the header.
               | StreamUnexpectedSituation String -- ^ An unexpected situation has come up parsing an ASN1 event stream.
               | ParsingHeaderFail String    -- ^ Parsing an invalid header.
               | ParsingPartial              -- ^ Parsing is not finished, there is construction unended.
               | TypeNotImplemented String   -- ^ Decoding of a type that is not implemented. Contribution welcome.
               | TypeDecodingFailed String   -- ^ Decoding of a knowed type failed.
               | PolicyFailed String String -- ^ Policy failed including the name of the policy and the reason.
               deriving (Typeable, Show, Eq)

instance Exception ASN1Error
