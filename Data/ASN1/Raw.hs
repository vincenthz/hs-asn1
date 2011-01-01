{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Data.ASN1.Raw
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing raw ASN1 serialization/derialization tools
--

module Data.ASN1.Raw
	(
	-- * ASN1 definitions
	  ASN1Class(..)
	, ASN1Tag
	, ASN1Length(..)
	, ASN1Header(..)
	, ASN1Err(..)
	-- * Enumerator events
	, ASN1Event(..)
	, iterateFile
	, iterateByteString
	, enumReadBytes
	, enumWriteBytes
	-- * serialize asn1 headers
	, getHeader
	, putHeader
	) where

import Data.Enumerator hiding (head, length, map)
import qualified Data.Enumerator as E
import Data.Enumerator.IO
import Data.Attoparsec.Enumerator
import Data.Attoparsec
import qualified Data.Attoparsec as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ASN1.Internal
import Control.Exception
import Data.Word
import Data.Bits
import Control.Monad
import Control.Monad.Trans (lift)
import Control.Applicative ((<|>), (<$>))

data ASN1Class =
	  Universal
	| Application
	| Context
	| Private
	deriving (Show,Eq,Ord,Enum)

type ASN1Tag = Int

data ASN1Length =
	  LenShort Int      -- ^ Short form with only one byte. length has to be < 127.
	| LenLong Int Int   -- ^ Long form of N bytes
	| LenIndefinite     -- ^ Length is indefinite expect an EOC in the stream to finish the type
	deriving (Show,Eq)

data ASN1Header = ASN1Header !ASN1Class !ASN1Tag !Bool !ASN1Length
	deriving (Show,Eq)

data ASN1Event =
	  Header ASN1Header     -- ^ ASN1 Header
	| Primitive !ByteString -- ^ Primitive
	| ConstructionBegin     -- ^ Constructed value start
	| ConstructionEnd       -- ^ Constructed value end
	deriving (Show,Eq)

data ASN1Err =
	  ASN1LengthDecodingLongContainsZero
	| ASN1PolicyFailed String String
	| ASN1NotImplemented String
	| ASN1Multiple [ASN1Err]
	| ASN1Misc String
	| ASN1ParsingPartial
	| ASN1ParsingFail
	deriving (Show, Eq)

{-| iterate over a file using a file enumerator. -}
iterateFile :: FilePath -> Iteratee ASN1Event IO a -> IO (Either SomeException a)
iterateFile path p = run (enumFile path $$ joinI (enumReadBytes $$ p))

{-| iterate over a lazy bytestring using a list enumerator over the bytestring chunks. -}
iterateByteString :: Monad m => L.ByteString -> Iteratee ASN1Event m a -> m (Either SomeException a)
iterateByteString bs p = run (enumList 1 (L.toChunks bs) $$ joinI (enumReadBytes $$ p))

{- parse state machine -}
data ParseState =
	  PSPrimitive Int
	| PSConstructing Int
	| PSConstructingEOC

{-| enumReadBytes parse bytestring and generate asn1event. -}
enumReadBytes :: Monad m => Enumeratee ByteString ASN1Event m a
enumReadBytes = checkDone $ \k -> k (Chunks []) >>== loop [0] []
	where
		loop !cs !ps = checkDone (go cs ps)
		go (n:[]) [] k = iterDesc >>= eofCheck k
				(\(c,e,nps) -> case nps of
					PSPrimitive _ -> k (Chunks [e]) >>== loop [c+n] [nps]
					_             -> k (Chunks [e, ConstructionBegin]) >>== loop [0, c+n] [nps]
				)

		go (n:cs) (PSPrimitive i:pss) k =
			iterPrim i >>= (\e -> k (Chunks [e]) >>== loop (n+i:cs) pss)

		go (n:m:cs) fps@(PSConstructing i:pss) k
			| n == i    = k (Chunks [ConstructionEnd]) >>== loop (n+m:cs) pss
			| otherwise = iterDesc >>= eofCheck k
				(\(c, e, nps) -> case nps of
					PSPrimitive _ -> k (Chunks [e]) >>== loop (n+c:m:cs) (nps:fps)
					_             -> k (Chunks [e, ConstructionBegin]) >>== loop (0:n+c:m:cs) (nps:fps)
				)

		go (n:m:cs) fps@(PSConstructingEOC:pss) k =
			iterDesc >>= eofCheck k
				(\(c, e, nps) -> case e of -- check if EOC or continue
					(Header (ASN1Header _ 0 _ _)) -> k (Chunks [ConstructionEnd]) >>== loop (c+n+m:cs) pss
					_                             -> k (Chunks [e]) >>== loop (n+c:m:cs) (nps:fps)
				)
		-- error case
		go _ _ k = k (Chunks []) >>== return

		eofCheck k _ Nothing  = k (Chunks []) >>== return
		eofCheck _ f (Just x) = f $! x

		iterDesc :: Monad m => Iteratee ByteString m (Maybe (Int, ASN1Event, ParseState))
		iterDesc = iterParser ((endOfInput >> return Nothing) <|> fmap Just parseHeaderEvent)

		iterPrim i = iterParser (fmap (Primitive) (A.take i))

{- parseHeaderEvent returns the asn1event header, the length parsed and the next parse state. -}
parseHeaderEvent :: Parser (Int, ASN1Event, ParseState)
parseHeaderEvent = do
	(lbytes, asn1header@(ASN1Header _ _ pc len)) <- parseHeader
	let ps = if pc
		-- constructed value(s)
		then case len of
			LenIndefinite -> PSConstructingEOC
			LenLong _ i   -> PSConstructing i
			LenShort i    -> PSConstructing i
		-- primitive value
		else case len of
			LenIndefinite -> error "cannot do indefinite primitive"
			LenLong _ i   -> PSPrimitive i
			LenShort i    -> PSPrimitive i
	return (lbytes, Header asn1header, ps)

{- parseHeader parse a asn1 header in an attoparsec context.
 - it returns the number of bytes parsed, the asn1event for this event -}
parseHeader :: Parser (Int, ASN1Header)
parseHeader = do
	(cl,pc,t1)      <- parseFirstWord <$> anyWord8
	(tagbytes, tag) <- if t1 == 0x1f then getTagLong else return (0, t1)
	(lenbytes, len) <- getLength
	return (1+tagbytes+lenbytes, ASN1Header cl tag pc len)

{- parse an header from a single bytestring. -}
getHeader :: ByteString -> Either ASN1Err ASN1Header
getHeader l = case parse parseHeader l of
	(Fail _ _ _) -> Left ASN1ParsingFail
	(Partial _)  -> Left ASN1ParsingPartial
	Done b r     -> if B.null b then Right (snd r) else Left ASN1ParsingPartial

{- parse the first word of an header -}
parseFirstWord :: Word8 -> (ASN1Class, Bool, ASN1Tag)
parseFirstWord w = (cl,pc,t1)
	where
		cl = toEnum $ fromIntegral $ (w `shiftR` 6)
		pc = testBit w 5
		t1 = fromIntegral (w .&. 0x1f)

{- when the first tag is 0x1f, the tag is in long form, where
 - we get bytes while the 7th bit is set. -}
getTagLong :: Parser (Int, ASN1Tag)
getTagLong = do
	t <- fromIntegral <$> anyWord8
	when (t == 0x80) $ error "not canonical encoding of tag"
	if testBit t 7
		then getNext 1 (clearBit t 7)
		else return (1, t)

	where getNext !blen !n = do
		t <- fromIntegral <$> anyWord8
		if testBit t 7
			then getNext (blen + 1) (n `shiftL` 7 + clearBit t 7)
			else return (blen + 1, n `shiftL` 7 + t)

{- get the asn1 length which is either short form if 7th bit is not set,
 - indefinite form is the 7 bit is set and every other bits clear,
 - or long form otherwise, where the next bytes will represent the length
 -}
getLength :: Parser (Int, ASN1Length)
getLength = do
	l1 <- fromIntegral <$> anyWord8
	if testBit l1 7
		then case clearBit l1 7 of
			0   -> return (1, LenIndefinite)
			len -> do
				lw <- A.take len
				return (1+len, LenLong len $ uintbs lw)
		else
			return (1, LenShort l1)
	where
		{- uintbs return the unsigned int represented by the bytes -}
		uintbs = B.foldl (\acc n -> (acc `shiftL` 8) + fromIntegral n) 0

{- | putIdentifier encode an ASN1 Identifier into a marshalled value -}
putHeader :: ASN1Header -> ByteString
putHeader (ASN1Header cl tag pc len) = B.pack
	( putFirstWord (cl, pc, if tag < 0x1f then tag else 0x1f)
	: (if tag >= 0x1f then putTagLong tag else [])
	++ putLength len
	)

{- put first word of a header -}
putFirstWord :: (ASN1Class, Bool, ASN1Tag) -> Word8
putFirstWord (cl,pc,t1) = (cli `shiftL` 6) .|. (pcval `shiftL` 5) .|. (fromIntegral t1 .&. 0x1f)
	where
		cli   = fromIntegral $ fromEnum cl
		pcval = if pc then 0x1 else 0x0

{- marshall helper for putIdentifier to serialize long tag number -}
putTagLong :: ASN1Tag -> [Word8]
putTagLong n = revSethighbits $ split7bits n
	where
		revSethighbits :: [Word8] -> [Word8]
		revSethighbits []     = []
		revSethighbits (x:xs) = reverse $ (x : map (\i -> setBit i 7) xs)
		split7bits i
			| i == 0    = []
			| i <= 0x7f = [ fromIntegral i ]
			| otherwise = fromIntegral (i .&. 0x7f) : split7bits (i `shiftR` 7)

{- | putLength encode a length into a ASN1 length.
 - see getLength for the encoding rules -}
putLength :: ASN1Length -> [Word8]
putLength (LenShort i)
	| i < 0 || i > 0x7f = error "putLength: short length is not between 0x0 and 0x80"
	| otherwise         = [fromIntegral i]
putLength (LenLong _ i)
	| i < 0     = error "putLength: long length is negative"
	| otherwise = lenbytes : lw
		where
			lw       = bytesOfUInt $ fromIntegral i
			lenbytes = fromIntegral (length lw .|. 0x80)
putLength (LenIndefinite) = [0x80]

{-| write Bytes of events enumeratee -}
enumWriteBytes :: Monad m => Enumeratee ASN1Event ByteString m a
enumWriteBytes (E.Continue k) = do
	x <- E.head
	case x of
		Nothing                -> return $ E.Continue k
		Just (Header hdr)      -> do
			nstep <- lift $ runIteratee $ k $ E.Chunks [putHeader hdr]
			enumWriteBytes nstep
		Just (Primitive p)     -> do
			nstep <- lift $ runIteratee $ k $ E.Chunks [p]
			enumWriteBytes nstep
		Just ConstructionBegin -> do
			nstep <- lift $ runIteratee $ k $ E.Chunks []
			enumWriteBytes nstep
		Just ConstructionEnd   -> do
			-- FIXME need to push an EOC when doing a indefinite block
			nstep <- lift $ runIteratee $ k $ E.Chunks []
			enumWriteBytes nstep
enumWriteBytes step = return step
