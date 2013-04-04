-- |
-- Module      : Data.ASN1.Stream
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
    , ObjectID
    , ASN1Object(..)
    ) where

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)
import Data.ASN1.BitArray
import Data.ASN1.OID
import Data.ASN1.Types.Lowlevel
import Data.ByteString (ByteString)

-- | Define the type of container
data ASN1ConstructionType = Sequence
                          | Set
                          | Container ASN1Class ASN1Tag
                          deriving (Show,Eq)

-- T61 encoding : http://www.mail-archive.com/asn1@asn1.org/msg00460.html

-- | Define all possible ASN1 String encoding.
data ASN1StringEncoding =
      IA5       -- ^ 128 characters equivalent to the ASCII alphabet
    | UTF8      -- ^ UTF8
    | General   -- ^ all registered graphic and character sets (see ISO 2375) plus SPACE and DELETE.
    | Graphic   -- ^ all registered G sets and SPACE
    | Numeric   -- ^ encoding containing numeric [0-9] and space
    | Printable -- ^ printable [a-z] [A-Z] [()+,-.?:/=] and space.
    | VideoTex  -- ^ CCITT's T.100 and T.101 character sets
    | Visible   -- ^ International ASCII printing character sets
    | T61       -- ^ teletext
    | UTF32     -- ^ UTF32
    | BMP       -- ^ UCS2
    deriving (Show,Eq)

data ASN1TimeType = TimeUTC | TimeGeneralized
                  deriving (Show,Eq)

-- | Define high level ASN1 object.
data ASN1 =
      Boolean Bool
    | IntVal  Integer
    | BitString BitArray
    | OctetString ByteString
    | Null
    | OID  ObjectID
    | Real Double
    | Enumerated Int
    | ASN1String ASN1StringEncoding ByteString
    | ASN1Time ASN1TimeType UTCTime (Maybe TimeZone)
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
