{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.ASN1.BinaryEncoding.Parse
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Generic parsing facility for ASN1.
--
module Data.ASN1.BinaryEncoding.Parse
    (
    -- * incremental parsing interfaces
      runParseState
    , isParseDone
    , newParseState
    , ParseState
    , ParseCursor
    -- * simple parsing interfaces
    , parseLBS
    , parseBS
    ) where

import Control.Arrow (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ASN1.Error
import Data.ASN1.Types
import Data.ASN1.Types.Lowlevel
import Data.ASN1.Get
import Data.ASN1.Serialize
import Data.Word
import Data.Maybe (fromJust)
import qualified Data.DList as DList
import Data.DList (DList)

-- | nothing means the parser stop this construction on
-- an ASN1 end tag, otherwise specify the position
-- where the construction terminate.
type ConstructionEndAt = Maybe Word64

data ParseExpect = ExpectHeader (Maybe (B.ByteString -> Result ASN1Header))
                 | ExpectPrimitive Word64 (Maybe (B.ByteString -> Result ByteString))

type ParsePosition = Word64

-- | represent the parsing state of an ASN1 stream.
--
-- * the stack of constructed elements.
-- * the next expected type.
-- * the position in the stream.
--
data ParseState = ParseState [ConstructionEndAt] ParseExpect ParsePosition

-- | create a new empty parse state. position is 0
newParseState :: ParseState
newParseState = ParseState [] (ExpectHeader Nothing) 0

isEOC :: ASN1Header -> Bool
isEOC (ASN1Header cl t _ _) = cl == Universal && t == 0

asn1LengthToConst :: ASN1Length -> Maybe Word64
asn1LengthToConst (LenShort n)  = Just $ fromIntegral n
asn1LengthToConst (LenLong _ n) = Just $ fromIntegral n
asn1LengthToConst LenIndefinite = Nothing

-- | Represent the events and state thus far.
type ParseCursor = (DList ASN1Event, ParseState)

-- | run incrementally the ASN1 parser on a bytestring.
-- the result can be either an error, or on success a list
-- of events, and the new parsing state.
runParseState :: ParseState -- ^ parser state
              -> ByteString -- ^ input data as bytes
              -> Either ASN1Error ParseCursor
runParseState = loop
     where
           loop iniState bs
                | B.null bs = terminateAugment ((DList.empty, iniState), bs) >>= (Right . fst)
                | otherwise = go iniState bs >>= terminateAugment
                                             >>= \((evs, newState), nbs) -> loop newState nbs
                                             >>= (Right . first (DList.append evs))

           terminateAugment ret@((!evs, ParseState stackEnd pe pos), r) =
                case stackEnd of
                    Just endPos:xs
                         | pos > endPos  -> Left StreamConstructionWrongSize
                         | pos == endPos -> terminateAugment ((DList.snoc evs ConstructionEnd, ParseState xs pe pos), r)
                         | otherwise     -> Right ret
                    _                    -> Right ret

           -- go get one element (either a primitive or a header) from the bytes
           -- and returns the new cursor and the remaining byte.
           go :: ParseState -> ByteString -> Either ASN1Error (ParseCursor, ByteString)
           go (ParseState stackEnd (ExpectHeader cont) pos) bs =
                case runGetHeader cont pos bs of
                     Fail s                 -> Left $ ParsingHeaderFail s
                     Partial f              -> Right ((DList.empty, ParseState stackEnd (ExpectHeader $ Just f) pos), B.empty)
                     Done hdr nPos remBytes
                        | isEOC hdr -> case stackEnd of
                                           []                  -> Right ((DList.empty, ParseState [] (ExpectHeader Nothing) nPos), remBytes)
                                           Just _:_            -> Left StreamUnexpectedEOC
                                           Nothing:newStackEnd -> Right ( ( DList.singleton ConstructionEnd
                                                                          , ParseState newStackEnd (ExpectHeader Nothing) nPos)
                                                                        , remBytes)
                        | otherwise -> case hdr of
                                       (ASN1Header _ _ True len)  ->
                                           let nEnd = (nPos +) `fmap` asn1LengthToConst len
                                           in Right ( ( DList.fromList [Header hdr,ConstructionBegin]
                                                      , ParseState (nEnd:stackEnd) (ExpectHeader Nothing) nPos)
                                                    , remBytes)
                                       (ASN1Header _ _ False LenIndefinite) -> Left StreamInfinitePrimitive
                                       (ASN1Header _ _ False len) ->
                                           let pLength = fromJust $ asn1LengthToConst len
                                           in if pLength == 0
                                                 then Right ( ( DList.fromList [Header hdr,Primitive B.empty]
                                                              , ParseState stackEnd (ExpectHeader Nothing) nPos)
                                                            , remBytes)
                                                 else Right ( ( DList.singleton (Header hdr)
                                                              , ParseState stackEnd (ExpectPrimitive pLength Nothing) nPos)
                                                            , remBytes)
           go (ParseState stackEnd (ExpectPrimitive len cont) pos) bs =
                case runGetPrimitive cont len pos bs of
                     Fail _               -> error "primitive parsing failed"
                     Partial f            -> Right ((DList.empty, ParseState stackEnd (ExpectPrimitive len $ Just f) pos), B.empty)
                     Done p nPos remBytes -> Right ((DList.singleton (Primitive p), ParseState stackEnd (ExpectHeader Nothing) nPos), remBytes)

           runGetHeader Nothing  = \pos -> runGetPos pos getHeader
           runGetHeader (Just f) = const f

           runGetPrimitive Nothing  n = \pos -> runGetPos pos (getBytes $ fromIntegral n)
           runGetPrimitive (Just f) _ = const f

-- | when no more input is available, it's important to check that the parser is
-- in a finish state too.
isParseDone :: ParseState -> Bool
isParseDone (ParseState [] (ExpectHeader Nothing) _) = True
isParseDone _                                        = False

-- | Parse one lazy bytestring and returns on success all ASN1 events associated.
parseLBS :: L.ByteString -> Either ASN1Error [ASN1Event]
parseLBS lbs = foldrEither process (DList.empty, newParseState) (L.toChunks lbs) >>= onSuccess
    where
          onSuccess :: (DList ASN1Event, ParseState) -> Either ASN1Error [ASN1Event]
          onSuccess (allEvs, finalState)
                  | isParseDone finalState = Right $ DList.toList $ allEvs
                  | otherwise              = Left ParsingPartial

          process :: (DList ASN1Event, ParseState) -> ByteString -> Either ASN1Error (DList ASN1Event, ParseState)
          process (pevs, cState) bs = runParseState cState bs >>= \(es, cState') ->
            let res = DList.append pevs es in res `seq` Right (res, cState')

          foldrEither :: (a -> ByteString -> Either ASN1Error a) -> a -> [ByteString] -> Either ASN1Error a
          foldrEither _ acc []     = Right acc
          foldrEither f acc (x:xs) = f acc x >>= \nacc -> foldrEither f nacc xs

-- | Parse one strict bytestring and returns on success all ASN1 events associated.
parseBS :: ByteString -> Either ASN1Error [ASN1Event]
parseBS bs = runParseState newParseState bs >>= onSuccess
    where onSuccess (evs, pstate)
                    | isParseDone pstate = Right (DList.toList evs)
                    | otherwise          = Left ParsingPartial
