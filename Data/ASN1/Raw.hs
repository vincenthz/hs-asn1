{-#  LANGUAGE GeneralizedNewtypeDeriving #-}
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
	( GetErr
	-- * get structure
	, runGetErr
	, runGetErrState
	, runGetErrInGet
	-- * ASN1 definitions
	, ASN1Err(..)
	, CheckFn
	, TagClass(..)
	, TagNumber
	, ValLength(..)
	, ValStruct(..)
	, Value(..)
	-- * get value from a Get structure
	, getValueCheck
	, getValue
	-- * put value in a Put structure
	, putValuePolicy
	, putValue
	) where

import Data.Bits
import Data.Int
import Data.ASN1.Internal
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.Word
import Control.Monad.Error
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

data TagClass =
	  Universal
	| Application
	| Context
	| Private
	deriving (Show, Eq)

data ValLength =
	  LenShort Int      -- ^ Short form with only one byte. length has to be < 127.
	| LenLong Int Int   -- ^ Long form of N bytes
	| LenIndefinite     -- ^ Length is indefinite expect an EOC in the stream to finish the type
	deriving (Show, Eq)

type TagNumber = Int
type TagConstructed = Bool
type Identifier = (TagClass, TagConstructed, TagNumber)

data ValStruct =
	  Primitive ByteString -- ^ Primitive of a strict value
	| Constructed [Value]  -- ^ Constructed of a list of values
	deriving (Show, Eq)

data Value = Value TagClass TagNumber ValStruct
	deriving (Show, Eq)

data ASN1Err =
	  ASN1LengthDecodingLongContainsZero
	| ASN1PolicyFailed String String
	| ASN1NotImplemented String
	| ASN1Multiple [ASN1Err]
	| ASN1Misc String
	deriving (Show, Eq)

type CheckFn = (TagClass, Bool, TagNumber) -> ValLength -> Maybe ASN1Err

instance Error ASN1Err where
	noMsg = ASN1Misc ""
	strMsg = ASN1Misc

newtype GetErr a = GE { runGE :: ErrorT ASN1Err Get a }
	deriving (Monad, MonadError ASN1Err)

instance Functor GetErr where
	fmap f = GE . fmap f . runGE

runGetErr :: GetErr a -> L.ByteString -> Either ASN1Err a
runGetErr = runGet . runErrorT . runGE

runGetErrState :: GetErr a -> L.ByteString -> Int64 -> Either ASN1Err (a, L.ByteString, Int64)
runGetErrState g l o =
	case runGetState (runErrorT $ runGE g) l o of
		(Left err, _, _)           -> Left err
		(Right v, datarem, parsed) -> Right (v, datarem, parsed)

runGetErrInGet :: GetErr a -> Get (Either ASN1Err a)
runGetErrInGet = runErrorT . runGE

liftGet :: Get a -> GetErr a
liftGet = GE . lift

geteWord8 :: GetErr Word8
geteWord8 = liftGet getWord8

geteBytes :: Int -> GetErr ByteString
geteBytes = liftGet . getBytes

{- marshall helper for getIdentifier to unserialize long tag number -}
getTagNumberLong :: GetErr TagNumber
getTagNumberLong = getNext 0 True
	where getNext n nz = do
		t <- fromIntegral `fmap` geteWord8
		when (nz && t == 0x80) $ throwError ASN1LengthDecodingLongContainsZero
		if testBit t 7
			then getNext (n `shiftL` 7 + clearBit t 7) False
			else return (n `shiftL` 7 + t)

{- marshall helper for putIdentifier to serialize long tag number -}
putTagNumberLong :: TagNumber -> Put
putTagNumberLong n = mapM_ putWord8 $ revSethighbits $ split7bits n
	where
		revSethighbits :: [Word8] -> [Word8]
		revSethighbits []     = []
		revSethighbits (x:xs) = reverse $ (x : map (\i -> setBit i 7) xs)
		split7bits i
			| i == 0    = []
			| i <= 0x7f = [ fromIntegral i ]
			| otherwise = fromIntegral (i .&. 0x7f) : split7bits (i `shiftR` 7)
		

{- | getIdentifier get an ASN1 encoded Identifier.
 - if the first 5 bytes value is less than 1f then it's encoded on 1 byte, otherwise
 - read bytes until the 8th bit is not set -}
getIdentifier :: GetErr Identifier
getIdentifier = do
	w <- geteWord8
	let cl =
		case (w `shiftR` 6) .&. 3 of
			0 -> Universal
			1 -> Application 
			2 -> Context
			3 -> Private
			_ -> Universal -- this cannot happens because of the .&. 3
	let pc = (w .&. 0x20) > 0
	let val = fromIntegral (w .&. 0x1f)
	vencoded <- if val < 0x1f then return val else getTagNumberLong
	return (cl, pc, vencoded)

{- | putIdentifier encode an ASN1 Identifier into a marshalled value -}
putIdentifier :: Identifier -> Put
putIdentifier (cl, pc, val) = do
	let cli = case cl of
		Universal   -> 0
		Application -> 1
		Context     -> 2
		Private     -> 3
	let pcval = if pc then 0x20 else 0x00
	if val < 0x1f
		then
			putWord8 $ fromIntegral $ (cli `shiftL` 6) .|. pcval .|. val
		else do
			putWord8 $ fromIntegral $ (cli `shiftL` 6) .|. pcval .|. 0x1f
			putTagNumberLong val

{- | getLength get the ASN1 encoded length of an item.
 - if less than 0x80 then it's encoded on 1 byte, otherwise
 - the first byte is the number of bytes to read as the length.
 - if the number of bytes is 0, then the length is indefinite,
 - and the content length is bounded by an EOC -}
getLength :: GetErr ValLength
getLength = do
	l1 <- geteWord8
	if testBit l1 7
		then case fromIntegral (clearBit l1 7) of
			0   -> return LenIndefinite
			len -> do
				lw <- geteBytes len
				return $ LenLong len (fromIntegral $ snd $ uintOfBytes lw)
		else
			return $ LenShort $ fromIntegral l1

{- | putLength encode a length into a ASN1 length.
 - see getLength for the encoding rules -}
putLength :: ValLength -> Put
putLength (LenShort i)
	| i < 0 || i > 0x7f = error "putLength: short length is not between 0x0 and 0x80"
	| otherwise         = putWord8 $ fromIntegral i

putLength (LenLong _ i)
	| i < 0     = error "putLength: long length is negative"
	| otherwise = putWord8 lenbytes >> mapM_ putWord8 lw
		where
			lw       = bytesOfUInt $ fromIntegral i
			lenbytes = fromIntegral (length lw .|. 0x80)
	
putLength (LenIndefinite) = putWord8 0x80

{- helper to getValue to build a constructed list of values when length is known -}
getValueConstructed :: CheckFn -> GetErr [Value]
getValueConstructed check = do
	remain <- liftGet remaining
	if remain > 0
		then liftM2 (:) (getValueCheck check) (getValueConstructed check)
		else return []

{- helper to getValue to build a constructed list of values when length is unknown -}
getValueConstructedUntilEOC :: CheckFn -> GetErr [Value]
getValueConstructedUntilEOC check = do
	o <- getValueCheck check
	case o of
		-- technically EOC should also match (Primitive B.empty) (LenShort 0)
		Value Universal 0 _ -> return []
		_                   -> liftM (o :) (getValueConstructedUntilEOC check)

getValueOfLength :: CheckFn -> Int -> Bool -> GetErr ValStruct
getValueOfLength check len pc = do
	b <- geteBytes len
	if pc
		then case runGetErr (getValueConstructed check) (L.fromChunks [b]) of
			Right x  -> return $ Constructed x
			Left err -> throwError err
		else
			return $ Primitive b

{- | getValueCheck decode an ASN1 value and check the values received through the check fn -}
getValueCheck :: CheckFn -> GetErr Value
getValueCheck check = do
	(tc, pc, tn) <- getIdentifier
	vallen <- getLength

	{- policy checker, if it returns an error, raise it -}
	case check (tc, pc, tn) vallen of
		Just err -> throwError err
		Nothing  -> return ()

	struct <- case vallen of
		LenIndefinite -> do
			unless pc $ throwError $ ASN1Misc "lenght indefinite not allowed with primitive"
			vs <- getValueConstructedUntilEOC check
			return $ Constructed vs
		(LenShort len)  -> getValueOfLength check len pc
		(LenLong _ len) -> getValueOfLength check len pc
	return $ Value tc tn struct

getValue :: GetErr Value
getValue = getValueCheck (\_ _ -> Nothing)

putValStruct :: ValStruct -> Put
putValStruct (Primitive x)   = putByteString x
putValStruct (Constructed l) = mapM_ putValue l

putValuePolicy :: (Value -> Int -> ValLength) -> Value -> Put
putValuePolicy policy v@(Value tc tn struct) = do
	let pc =
		case struct of
			Primitive _   -> False
			Constructed _ -> True
	putIdentifier (tc, pc, tn)
	let content = runPut (putValStruct struct)
	let len = fromIntegral $ L.length content 
	let lenEncoded = policy v len
	putLength lenEncoded
	putLazyByteString content
	case lenEncoded of
		LenIndefinite -> putValue $ Value Universal 0x0 (Primitive B.empty)
		_             -> return ()

{- | putValue encode an ASN1 value using the shortest definite length -}
putValue :: Value -> Put
putValue = putValuePolicy (\_ len -> if len < 0x80 then LenShort len else LenLong 0 len)
