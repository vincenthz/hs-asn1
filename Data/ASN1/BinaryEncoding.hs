-- |
-- Module      : Data.ASN1.BinaryEncoding
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing ASN1 BER and DER specification encoding/decoding.
--
{-# LANGUAGE EmptyDataDecls #-}
module Data.ASN1.BinaryEncoding
    ( BER(..)
    , DER(..)
    ) where

import Data.ASN1.Stream
import Data.ASN1.Types
import Data.ASN1.Types.Lowlevel
import Data.ASN1.Error
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding.Parse
import Data.ASN1.BinaryEncoding.Writer
import Data.ASN1.Prim
import qualified Control.Exception as E

-- | Basic Encoding Rules (BER)
data BER = BER

-- | Distinguished Encoding Rules (DER)
data DER = DER

instance ASN1DecodingRepr BER where
    decodeASN1Repr _ lbs = decodeEventASN1Repr (const Nothing) `fmap` parseLBS lbs

instance ASN1Decoding BER where
    decodeASN1 _ lbs = (map fst . decodeEventASN1Repr (const Nothing)) `fmap` parseLBS lbs

instance ASN1DecodingRepr DER where
    decodeASN1Repr _ lbs = decodeEventASN1Repr checkDER `fmap` parseLBS lbs

instance ASN1Decoding DER where
    decodeASN1 _ lbs = (map fst . decodeEventASN1Repr checkDER) `fmap` parseLBS lbs

instance ASN1Encoding DER where
    encodeASN1 _ l = toLazyByteString $ encodeToRaw l

decodeConstruction :: ASN1Header -> ASN1ConstructionType
decodeConstruction (ASN1Header Universal 0x10 _ _) = Sequence
decodeConstruction (ASN1Header Universal 0x11 _ _) = Set
decodeConstruction (ASN1Header c t _ _)            = Container c t

decodeEventASN1Repr :: (ASN1Header -> Maybe ASN1Error) -> [ASN1Event] -> [ASN1Repr]
decodeEventASN1Repr checkHeader l = loop [] l
    where loop _ []     = []
          loop acc (h@(Header hdr@(ASN1Header _ _ True _)):ConstructionBegin:xs) =
                let ctype = decodeConstruction hdr in
                case checkHeader hdr of
                    Nothing  -> (Start ctype,[h,ConstructionBegin]) : loop (ctype:acc) xs
                    Just err -> E.throw err
          loop acc (h@(Header hdr@(ASN1Header _ _ False _)):p@(Primitive prim):xs) =
                case checkHeader hdr of
                    Nothing -> case decodePrimitive hdr prim of
                        Left err  -> E.throw err
                        Right obj -> (obj, [h,p]) : loop acc xs
                    Just err -> E.throw err
          loop (ctype:acc) (ConstructionEnd:xs) = (End ctype, [ConstructionEnd]) : loop acc xs
          loop _ (x:_) = E.throw $ StreamUnexpectedSituation (show x)

-- | DER header need to be all of finite size and of minimum possible size.
checkDER :: ASN1Header -> Maybe ASN1Error
checkDER (ASN1Header _ _ _ len) = checkLength len
    where checkLength :: ASN1Length -> Maybe ASN1Error
          checkLength LenIndefinite = Just $ PolicyFailed "DER" "indefinite length not allowed"
          checkLength (LenShort _)  = Nothing
          checkLength (LenLong n i)
              | n == 1 && i < 0x80  = Just $ PolicyFailed "DER" "long length should be a short length"
              | n == 1 && i >= 0x80 = Nothing
              | otherwise           = if i >= 2^((n-1)*8) && i < 2^(n*8)
                  then Nothing
                  else Just $ PolicyFailed "DER" "long length is not shortest"

encodeToRaw :: [ASN1] -> [ASN1Event]
encodeToRaw = concatMap writeTree . mkTree
    where writeTree (p@(Start _),children) = snd $ encodeConstructed p children
          writeTree (p,_)                  = snd $ encodePrimitive p

          mkTree []           = []
          mkTree (x@(Start _):xs) =
              let (tree, r) = spanEnd 0 xs
               in (x,tree):mkTree r
          mkTree (p:xs)       = (p,[]) : mkTree xs

          spanEnd :: Int -> [ASN1] -> ([ASN1], [ASN1])
          spanEnd _ []             = ([], [])
          spanEnd 0 (x@(End _):xs) = ([x], xs)
          spanEnd lvl (x:xs)       = case x of
                    Start _ -> let (ys, zs) = spanEnd (lvl+1) xs in (x:ys, zs)
                    End _   -> let (ys, zs) = spanEnd (lvl-1) xs in (x:ys, zs)
                    _       -> let (ys, zs) = spanEnd lvl xs in (x:ys, zs)
