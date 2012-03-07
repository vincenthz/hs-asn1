-- |
-- Module      : Data.ASN1.BER
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing ASN1 BER specification serialization/derialization tools
--
module Data.ASN1.BER
	( ASN1Class(..)
	, ASN1(..)
	, ASN1ConstructionType(..)

	-- * enumeratee to transform between ASN1 and raw
	, enumReadRawRepr
	, enumReadRaw
	, enumWriteRaw

	-- * enumeratee to transform between ASN1 and bytes
	, enumReadBytes
	, enumWriteBytes

	-- * iterate over common representation to an ASN1 stream
	, iterateFile
	, iterateByteString
	, iterateByteStringRepr
	, iterateEvents
	, iterateEventsRepr

	-- * BER serialize functions
	, decodeASN1EventsRepr
	, decodeASN1Events
	, encodeASN1Events

	, decodeASN1Stream
	, decodeASN1StreamRepr
	, encodeASN1Stream
	) where

import Data.ASN1.Raw (ASN1Header(..), ASN1Class(..), ASN1Err(..))
import qualified Data.ASN1.Raw as Raw

import Data.ASN1.Stream
import Data.ASN1.Prim

import Control.Monad.Identity
import Control.Exception

import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)

import Data.Enumerator.Binary (enumFile)
import Data.Enumerator (Iteratee(..), Enumeratee, ($$), (>>==))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

decodeConstruction :: ASN1Header -> ASN1ConstructionType
decodeConstruction (ASN1Header Universal 0x10 _ _) = Sequence
decodeConstruction (ASN1Header Universal 0x11 _ _) = Set
decodeConstruction (ASN1Header c t _ _)            = Container c t

{- | enumerate from 'Raw.ASN1Event' to an 'ASN1Repr' (ASN1 augmented by a list of raw asn1 events)
 -}
enumReadRawRepr :: Monad m => Enumeratee Raw.ASN1Event ASN1Repr m a
enumReadRawRepr = E.checkDone $ \k -> k (E.Chunks []) >>== loop []
	where
		loop l = E.checkDone $ go l
		go l k = EL.head >>= \x -> case x of
			Nothing -> if l == [] then k (E.Chunks []) >>== return else E.throwError (Raw.ASN1ParsingPartial)
			Just el -> p l k el

		{- on construction end, we pop the list context -}
		p l k Raw.ConstructionEnd = k (E.Chunks [head l]) >>== loop (tail l)

		{- on header with construction, we pop the next element in the enumerator and
		 - expect a ConstructionBegin. the list context is prepended by the new construction -}
		p l k el@(Raw.Header hdr@(ASN1Header _ _ True _)) = EL.head >>= \z -> case z of
			Just el2@Raw.ConstructionBegin ->
				let ctype = decodeConstruction hdr in
				k (E.Chunks [(Start ctype, [el,el2])]) >>== loop ((End ctype,[Raw.ConstructionEnd]) : l)
			Just _  -> E.throwError (Raw.ASN1ParsingFail "expecting construction")
			Nothing -> E.throwError (Raw.ASN1ParsingFail "expecting construction, got EOF")

		{- on header with primtive, we pop the next element in the enumerator and
		 - expect a Primitive -}
		p l k el@(Raw.Header hdr@(ASN1Header _ _ False _)) = EL.head >>= \z -> case z of
			Just el2@(Raw.Primitive prim) ->
				let (Right pr) = decodePrimitive hdr prim in
				k (E.Chunks [(pr, [el,el2])]) >>== loop l
			Just _  -> E.throwError (Raw.ASN1ParsingFail "expecting primitive")
			Nothing -> E.throwError (Raw.ASN1ParsingFail "expecting primitive, got EOF")

		p _ _ _ = E.throwError (Raw.ASN1ParsingFail "boundary not a header")

{- | enumeratee from 'Raw.ASN1Event to 'ASN1'
 -
 - it's the enumerator equivalent:
 - @enumReadRaw = map fst . enumReadRawRepr@
 -}
enumReadRaw :: Monad m => Enumeratee Raw.ASN1Event ASN1 m a
enumReadRaw = \f -> E.joinI (enumReadRawRepr $$ (E.map fst f))

{- | enumWriteRaw is an enumeratee from asn1 to raw events -}
enumWriteRaw :: Monad m => Enumeratee ASN1 Raw.ASN1Event m a
enumWriteRaw = \f -> E.joinI (enumWriteTree $$ (enumWriteTreeRaw f))

enumWriteTree :: Monad m => Enumeratee ASN1 (ASN1, [ASN1]) m a
enumWriteTree = do
	E.checkDone $ \k -> k (E.Chunks []) >>== loop
	where
		loop = E.checkDone $ go
		go k = EL.head >>= \x -> case x of
			Nothing          -> k (E.Chunks []) >>== return
			Just n@(Start _) -> consumeTillEnd >>= \y -> k (E.Chunks [(n, y)] ) >>== loop
			Just p           -> k (E.Chunks [(p, [])] ) >>== loop

		consumeTillEnd :: Monad m => Iteratee ASN1 m [ASN1]
		consumeTillEnd = E.liftI $ step (1 :: Int) id where
			step l acc chunk = case chunk of
				E.Chunks [] -> E.Continue $ E.returnI . step l acc
				E.Chunks xs -> do
					let (ys, zs) = spanEnd l xs
					let nbend = length $ filter isEnd ys
					let nbstart = length $ filter isStart ys
					let nl = l - nbend + nbstart
					if nl == 0
						then E.Yield (acc ys) (E.Chunks zs)
						else E.Continue $ E.returnI . (step nl $ acc . (ys ++))
				E.EOF       -> E.Yield (acc []) E.EOF

			spanEnd :: Int -> [ASN1] -> ([ASN1], [ASN1])
			spanEnd _ []               = ([], [])
			spanEnd 0 (x@(End _):xs)   = ([x], xs)
			spanEnd 0 (x@(Start _):xs) = let (ys, zs) = spanEnd 1 xs in (x:ys, zs)
			spanEnd 0 (x:xs)           = let (ys, zs) = spanEnd 0 xs in (x:ys, zs)
			spanEnd l (x:xs)           = case x of
				Start _ -> let (ys, zs) = spanEnd (l+1) xs in (x:ys, zs)
				End _   -> let (ys, zs) = spanEnd (l-1) xs in (x:ys, zs)
				_       -> let (ys, zs) = spanEnd l xs in (x:ys, zs)

			isStart (Start _) = True
			isStart _         = False
			isEnd (End _)     = True
			isEnd _           = False


enumWriteTreeRaw :: Monad m => Enumeratee (ASN1, [ASN1]) Raw.ASN1Event m a
enumWriteTreeRaw = E.concatMap writeTree
	where writeTree (p,children) = snd $ case p of
		Start _ -> encodeConstructed p children
		_       -> encodePrimitive p

{-| enumReadBytes is an enumeratee converting from bytestring to ASN1
  it transforms chunks of bytestring into chunks of ASN1 objects -}
enumReadBytes :: Monad m => Enumeratee ByteString ASN1 m a
enumReadBytes = \f -> E.joinI (Raw.enumReadBytes $$ (enumReadRaw f))

{-| enumReadBytes is an enumeratee converting from bytestring to ASN1
  it transforms chunks of bytestring into chunks of ASN1 objects -}
enumReadBytesRepr :: Monad m => Enumeratee ByteString ASN1Repr m a
enumReadBytesRepr = \f -> E.joinI (Raw.enumReadBytes $$ (enumReadRawRepr f))

{-| enumWriteBytes is an enumeratee converting from ASN1 to bytestring.
  it transforms chunks of ASN1 objects into chunks of bytestring  -}
enumWriteBytes :: Monad m => Enumeratee ASN1 ByteString m a
enumWriteBytes = \f -> E.joinI (enumWriteRaw $$ (Raw.enumWriteBytes f))

{-| iterate over a file using a file enumerator. -}
iterateFile :: FilePath -> Iteratee ASN1 IO a -> IO (Either SomeException a)
iterateFile path p = E.run (enumFile path $$ E.joinI $ enumReadBytes $$ p)

{-| iterate over a bytestring using a list enumerator over each chunks -}
iterateByteString :: Monad m => L.ByteString -> Iteratee ASN1 m a -> m (Either SomeException a)
iterateByteString bs p = E.run (E.enumList 1 (L.toChunks bs) $$ E.joinI $ enumReadBytes $$ p)

{-| iterate over a bytestring using a list enumerator over each chunks -}
iterateByteStringRepr :: Monad m => L.ByteString -> Iteratee ASN1Repr m a -> m (Either SomeException a)
iterateByteStringRepr bs p = E.run (E.enumList 1 (L.toChunks bs) $$ E.joinI $ enumReadBytesRepr $$ p)

{-| iterate over asn1 events using a list enumerator over each chunks -}
iterateEvents :: Monad m => [Raw.ASN1Event] -> Iteratee ASN1 m a -> m (Either SomeException a)
iterateEvents evs p = E.run (E.enumList 8 evs $$ E.joinI $ enumReadRaw $$ p)

{-| iterate over asn1 events using a list enumerator over each chunks -}
iterateEventsRepr :: Monad m => [Raw.ASN1Event] -> Iteratee ASN1Repr m a -> m (Either SomeException a)
iterateEventsRepr evs p = E.run (E.enumList 8 evs $$ E.joinI $ enumReadRawRepr $$ p)

{- helper to transform a Someexception from the enumerator to an ASN1Err if possible -}
wrapASN1Err :: Either SomeException a -> Either ASN1Err a
wrapASN1Err (Left err) = Left (maybe (ASN1ParsingFail $ show err) id $ fromException err)
wrapASN1Err (Right x)  = Right x

{-| decode a list of raw ASN1Events into a stream of ASN1 types -}
decodeASN1Events :: [Raw.ASN1Event] -> Either ASN1Err [ASN1]
decodeASN1Events evs = wrapASN1Err $ runIdentity (iterateEvents evs EL.consume)

{-| decode a list of raw ASN1Events into a stream of ASN1Repr types -}
decodeASN1EventsRepr :: [Raw.ASN1Event] -> Either ASN1Err [ASN1Repr]
decodeASN1EventsRepr evs = wrapASN1Err $ runIdentity (iterateEventsRepr evs EL.consume)

{-| decode a lazy bytestring as an ASN1 stream -}
decodeASN1Stream :: L.ByteString -> Either ASN1Err [ASN1]
decodeASN1Stream l = wrapASN1Err $ runIdentity (iterateByteString l EL.consume)

{-| decode a lazy bytestring as an ASN1repr stream -}
decodeASN1StreamRepr :: L.ByteString -> Either ASN1Err [ASN1Repr]
decodeASN1StreamRepr l = wrapASN1Err $ runIdentity (iterateByteStringRepr l EL.consume)

{-| encode an ASN1 Stream as raw ASN1 Events -}
encodeASN1Events :: [ASN1] -> Either ASN1Err [Raw.ASN1Event]
encodeASN1Events o = wrapASN1Err $ runIdentity run
	where run = E.run (E.enumList 8 o $$ E.joinI $ enumWriteRaw $$ EL.consume)

{-| encode an ASN1 Stream as lazy bytestring -}
encodeASN1Stream :: [ASN1] -> Either ASN1Err L.ByteString
encodeASN1Stream l = either Left (Right . L.fromChunks) $ wrapASN1Err $ runIdentity run
	where run = E.run (E.enumList 1 l $$ E.joinI $ enumWriteBytes $$ EL.consume)
