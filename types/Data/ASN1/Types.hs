-- |
-- Module      : Data.ASN1.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ASN1.Types
    ( ASN1(..)
    , ASN1S
    , ASN1Class(..)
    , ASN1Tag
    , ASN1ConstructionType(..)
    , ASN1StringEncoding(..)
    , ASN1TimeType(..)
    , ASN1Object(..)
    , ASN1CharacterString(..)
    , asn1CharacterString
    , asn1CharacterToString
    , module Data.ASN1.OID
    ) where

import Data.Hourglass
import Data.ASN1.BitArray
import Data.ASN1.OID
import Data.ASN1.Types.Lowlevel
import Data.ASN1.Types.String
import Data.ByteString (ByteString)

-- | Define the type of container
data ASN1ConstructionType = Sequence
                          | Set
                          | Container ASN1Class ASN1Tag
                          deriving (Show,Eq)

-- | Different ASN1 time representation
data ASN1TimeType = TimeUTC         -- ^ ASN1 UTCTime Type: limited between 1950-2050
                  | TimeGeneralized -- ^ ASN1 GeneralizedTime Type
                  deriving (Show,Eq,Ord)

-- | Define high level ASN1 object.
data ASN1 =
      Boolean Bool
    | IntVal  Integer
    | BitString BitArray
    | OctetString ByteString
    | Null
    | OID  OID
    | Real Double
    | Enumerated Integer
    | ASN1String ASN1CharacterString
    | ASN1Time ASN1TimeType DateTime (Maybe TimezoneOffset)
    | Other ASN1Class ASN1Tag ByteString
    | Start ASN1ConstructionType
    | End   ASN1ConstructionType
    deriving (Show, Eq)

-- | represent a chunk of ASN1 Stream.
-- this is equivalent to ShowS but for an ASN1 Stream.
type ASN1S = [ASN1] -> [ASN1]

-- | Define an object that can be converted to and from ASN.1
class ASN1Object a where
    -- | transform an object into a chunk of ASN1 stream.
    toASN1   :: a      -> ASN1S

    -- | returns either an object along the remaining ASN1 stream,
    -- or an error.
    fromASN1 :: [ASN1] -> Either String (a, [ASN1])
