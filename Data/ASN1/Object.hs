-- |
-- Module      : Data.ASN1.Object
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ASN1.Object
    ( ASN1Object(..)
    ) where

import Data.ASN1.Stream

-- | an object that can be marshalled from and to ASN1
class ASN1Object a where
    -- | transform an object into an ASN1 stream.
    toASN1   :: a      -> [ASN1]
    -- | returns either an object along the remaining ASN1 stream,
    -- or an error.
    fromASN1 :: [ASN1] -> Either String (a, [ASN1])
