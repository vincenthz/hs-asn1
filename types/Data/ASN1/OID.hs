-- |
-- Module      : Data.ASN1.OID
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE DeriveDataTypeable #-}
module Data.ASN1.OID
    ( OID
    -- * classes
    , OIDable(..)
    , OIDNameable(..)
    ) where

-- | Standard ASN.1 Object ID (OID)
type OID = [Integer]

-- | Class of things that have an Object ID
class OIDable a where
    -- | return the object ID of an Object from the ObjectIdentifiable class.
    getObjectID :: a -> OID

-- | Class of things that can be named by Object ID
class OIDNameable a where
    -- | Try to convert an OID into an Object
    fromObjectID :: OID -> Maybe a
