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

import Control.Monad.Catch
import Control.Monad.State
import Data.ASN1.Stream
import Data.ASN1.Types
import Data.ASN1.Types.Lowlevel
import Data.ASN1.Error
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding.Parse
import Data.ASN1.BinaryEncoding.Writer
import Data.ASN1.Prim
import Data.Conduit
import qualified Control.Exception as E
import qualified Data.Conduit.Lift as C
import qualified Data.Conduit.List as C

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

instance ASN1DecodingReprConduit DER where
  decodeASN1ReprConduit _ = parseConduit =$= decodeEventASN1ReprConduit checkDER

instance ASN1DecodingConduit DER where
  decodeASN1Conduit _ = parseConduit =$= decodeEventASN1ReprConduit checkDER =$= C.map fst

decodeConstruction :: ASN1Header -> ASN1ConstructionType
decodeConstruction (ASN1Header Universal 0x10 _ _) = Sequence
decodeConstruction (ASN1Header Universal 0x11 _ _) = Set
decodeConstruction (ASN1Header c t _ _)            = Container c t

decodeEventASN1ReprConduit :: MonadThrow m => (ASN1Header -> Maybe ASN1Error) -> Conduit ASN1Event m ASN1Repr
decodeEventASN1ReprConduit checkHeader = C.evalStateC [] . awaitForever $ \ev -> do
  st <- lift get
  next <- await
  case (ev, next) of
    (h@(Header hdr@(ASN1Header _ _ True _)), Just ConstructionBegin) ->
      let ctype = decodeConstruction hdr in
      case checkHeader hdr of
        Nothing -> do
          yield (Start ctype, [h, ConstructionBegin])
          lift $ put (ctype:st)
        Just err -> throwM err
    (h@(Header hdr@(ASN1Header _ _ False _)), Just (p@(Primitive prim))) ->
      case checkHeader hdr of
        Nothing -> case decodePrimitive hdr prim of
          Left err -> throwM err
          Right obj -> yield (obj, [h, p])
        Just err -> throwM err
    (ConstructionEnd, _) -> do
      ctype <- lift $ case st of
        (x:xs) -> put xs >> return x
        _ -> throwM $ StreamUnexpectedSituation (show ConstructionEnd)
      yield (End ctype, [ConstructionEnd])
      mapM_ leftover next
    (x, _) ->
      throwM $ StreamUnexpectedSituation (show x)

decodeEventASN1Repr :: (ASN1Header -> Maybe ASN1Error) -> [ASN1Event] -> [ASN1Repr]
decodeEventASN1Repr checkHeader l =
  case C.sourceList l =$= decodeEventASN1ReprConduit checkHeader $$ C.consume of
    Left err -> E.throw err
    Right x -> x

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
