-- |
-- Module      : Data.ASN1.Stream
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module Data.ASN1.Stream
    ( ASN1Repr
    , getConstructedEnd
    , getConstructedEndRepr
    ) where

import Data.ASN1.Types
import Data.ASN1.Types.Lowlevel

{- associate a list of asn1 event with an ASN1 type.
 - it's sometimes required to know the exact byte sequence leading to an ASN1 type:
 - eg: cryptographic signature -}
type ASN1Repr = (ASN1, [ASN1Event])

getConstructedEnd :: Int -> [ASN1] -> ([ASN1],[ASN1])
getConstructedEnd _ xs@[]                = (xs, [])
getConstructedEnd i ((x@(Start _)):xs)   = let (yz, zs) = getConstructedEnd (i+1) xs in (x:yz,zs)
getConstructedEnd i ((x@(End _)):xs)
    | i == 0    = ([], xs)
    | otherwise = let (ys, zs) = getConstructedEnd (i-1) xs in (x:ys,zs)
getConstructedEnd i (x:xs)               = let (ys, zs) = getConstructedEnd i xs in (x:ys,zs)

getConstructedEndRepr :: [ASN1Repr] -> ([ASN1Repr],[ASN1Repr])
getConstructedEndRepr = g
    where g []                 = ([], [])
          g (x@(Start _,_):xs) = let (ys, zs) = getEnd 1 xs in (x:ys, zs)
          g (x:xs)             = ([x],xs)

          getEnd :: Int -> [ASN1Repr] -> ([ASN1Repr],[ASN1Repr])
          getEnd _ []                    = ([], [])
          getEnd 0 xs                    = ([], xs)
          getEnd i ((x@(Start _, _)):xs) = let (ys, zs) = getEnd (i+1) xs in (x:ys,zs)
          getEnd i ((x@(End _, _)):xs)   = let (ys, zs) = getEnd (i-1) xs in (x:ys,zs)
          getEnd i (x:xs)                = let (ys, zs) = getEnd i xs in (x:ys,zs)
