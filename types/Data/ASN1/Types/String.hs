-- |
-- Module      : Data.ASN1.Types.String
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Different String types available in ASN1
--
module Data.ASN1.Types.String
    ( ASN1StringEncoding(..)
    , ASN1CharacterString(..)
    ) where

import Data.ByteString (ByteString)

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
    | Character -- ^ Character
    | BMP       -- ^ UCS2
    deriving (Show,Eq,Ord)

-- | ASN1 Character String with encoding
data ASN1CharacterString = ASN1CharacterString
    { characterEncoding         :: ASN1StringEncoding
    , getCharacterStringRawData :: ByteString
    } deriving (Show,Eq,Ord)

