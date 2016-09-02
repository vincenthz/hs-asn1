{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.ASN1.Stream
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ASN1.Stream
    ( ASN1(..)
    , ASN1Class(..)
    , ASN1Tag
    , ASN1Repr
    , ASN1ConstructionType(..)
    , getConstructedEnd
    , getConstructedEndRepr
    ) where

import Data.ASN1.BitArray
import Data.ASN1.Types
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

-- | Define the type of container
data ASN1ConstructionType = Sequence
                          | Set
                          | Container ASN1Class ASN1Tag
                          deriving (Show,Eq)

-- | Define high level ASN1 object.
data ASN1 = Boolean {-# UNPACK #-} !Bool
          | IntVal {-# UNPACK #-} !Integer
          | BitString BitArray
          | OctetString L.ByteString
          | Null
          | OID [Integer]
          | Real {-# UNPACK #-} !Double
          | Enumerated
          | UTF8String String
          | NumericString L.ByteString
          | PrintableString String
          | T61String String
          | VideoTexString L.ByteString
          | IA5String String
          | UTCTime (Int, Int, Int, Int, Int, Int, Bool)
          | GeneralizedTime (Int, Int, Int, Int, Int, Int, Bool)
          | GraphicString L.ByteString
          | VisibleString L.ByteString
          | GeneralString L.ByteString
          | UniversalString String
          | CharacterString L.ByteString
          | BMPString String
          | Other ASN1Class ASN1Tag ByteString
          | Start ASN1ConstructionType
          | End ASN1ConstructionType
          deriving (Show, Eq)

{- associate a list of asn1 event with an ASN1 type.
 - it's sometimes required to know the exact byte sequence leading to an ASN1 type:
 - eg: cryptographic signature -}
type ASN1Repr = (ASN1, [ASN1Event])

getConstructedEnd :: Int -> [ASN1] -> ([ASN1],[ASN1])
getConstructedEnd _ xs@[]                = (xs, [])
getConstructedEnd !i ((x@(Start _)):xs)   = let (yz, zs) = getConstructedEnd (i+1) xs
                                                acc      = x:yz
                                                in acc `seq` (acc,zs)
getConstructedEnd !i ((x@(End _)):xs)
    | i == 0    = ([], xs)
    | otherwise = let (ys, zs) = getConstructedEnd (i-1) xs
                      acc      = x:ys
                      in acc `seq` (acc,zs)
getConstructedEnd !i (x:xs)               = let (ys, zs) = getConstructedEnd i xs
                                                acc      = x:ys
                                                in acc `seq` (acc,zs)

getConstructedEndRepr :: [ASN1Repr] -> ([ASN1Repr],[ASN1Repr])
getConstructedEndRepr = g
    where g []                 = ([], [])
          g (x@(Start _,_):xs) = let (ys, zs) = getEnd 1 xs in (x:ys, zs)
          g (x:xs)             = ([x],xs)

          getEnd :: Int -> [ASN1Repr] -> ([ASN1Repr],[ASN1Repr])
          getEnd _ []                    = ([], [])
          getEnd 0 xs                    = ([], xs)
          getEnd !i ((x@(Start _, _)):xs) = let (ys, zs) = getEnd (i+1) xs
                                                acc      = x:ys
                                                in acc `seq` (acc,zs)
          getEnd !i ((x@(End _, _)):xs)   = let (ys, zs) = getEnd (i-1) xs
                                                acc      = x:ys
                                                in acc `seq` (acc,zs)
          getEnd !i (x:xs)                = let (ys, zs) = getEnd i xs
                                                acc      = x:ys
                                                in acc `seq` (acc,zs)
