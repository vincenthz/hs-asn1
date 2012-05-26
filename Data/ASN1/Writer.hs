-- |
-- Module      : Data.ASN1.Writer
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Serialize events for streaming.
--
module Data.ASN1.Writer
    ( toByteString
    , toLazyByteString
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.ASN1.Types

toByteString :: [ASN1Event] -> ByteString
toByteString = undefined

toLazyByteString :: [ASN1Event] -> L.ByteString
toLazyByteString = undefined


