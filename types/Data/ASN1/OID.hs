-- |
-- Module      : Data.ASN1.OID
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE DeriveDataTypeable #-}
module Data.ASN1.OID
    ( ObjectID
    -- * ObjectID accessors
    , oid
    , getObjectIDNodes
    , EmptyObjectID
    -- * class
    , ObjectIdentifiable(..)
    ) where

import qualified Control.Exception as E
import Data.Data

-- | exception raised if trying to supply an empty OID.
data EmptyObjectID = EmptyObjectID
    deriving (Show,Eq,Typeable)

instance E.Exception EmptyObjectID

-- | Standard ASN.1 Object ID (OID)
newtype ObjectID = ObjectID
    { getObjectIDNodes :: [Integer] -- ^ return the successive numbers of the node
    } deriving (Show,Eq,Ord,Data,Typeable)

-- | create an Object ID (OID)
oid :: [Integer] -> ObjectID
oid []    = E.throw EmptyObjectID
oid nodes = ObjectID nodes

-- | Class of things that have an Object ID
class ObjectIdentifiable a where
    -- | return the object ID of an Object from the ObjectIdentifiable class.
    getObjectID :: a -> ObjectID
