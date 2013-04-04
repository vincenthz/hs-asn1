-- |
-- Module      : Data.ASN1.Types.Lowlevel
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE BangPatterns #-}
module Data.ASN1.Types.Lowlevel
    (
    -- * Raw types
      ASN1Class(..)
    , ASN1Tag
    , ASN1Length(..)
    , ASN1Header(..)
    -- * Events types
    , ASN1Event(..)
    ) where

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
