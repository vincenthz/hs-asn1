-- |
-- Module      : Data.ASN1.Representation
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown

-- ASN1 High Level representation

module Data.ASN1.Representation
	( ASN1Element(..)
	, ASN1StructType(..)
	, ASN1Tree(..)
	, ASN1Rules(..)
	-- * common encoding and decoding rules
	, rulesBER
	, rulesDER
	-- * serialization from and to events 
	, decodeASN1WithEvents
	, decodeASN1
	, decodeFromBytes
	, encodeASN1
	, encodeToBytes
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)
import Data.ASN1.Event
import Data.ASN1.Prim

import Control.Applicative ((<$>))
import Control.Monad.Error

data ASN1Element =
	  Boolean Bool
	| IntVal Integer
	| BitString Int ByteString
	| OctetString ByteString
	| Null
	| OID [Integer]
	| Real Double
	| Enumerated
	| UTF8String String
	| NumericString ByteString
	| PrintableString String
	| T61String String
	| VideoTexString ByteString
	| IA5String String
	| UTCTime (Int, Int, Int, Int, Int, Int, Bool)
	| GeneralizedTime (Int, Int, Int, Int, Int, Int, Bool)
	| GraphicString ByteString
	| VisibleString ByteString
	| GeneralString ByteString
	| UniversalString String
	| CharacterString ByteString
	| BMPString String
	| Other ASN1Class ASN1Tag ByteString
	deriving (Show, Eq)

data ASN1StructType =
	  TypeSequence
	| TypeSet
	| TypeContainer ASN1Class ASN1Tag
	deriving (Show,Eq)

data ASN1Tree a =
	  Sequence [ASN1Tree a] a
	| Set [ASN1Tree a] a
	| Container ASN1Class ASN1Tag [ASN1Tree a] a
	| Element ASN1Element a
	deriving (Show)

instance Eq (ASN1Tree a) where
	(Sequence c1 _) == (Sequence c2 _)                 = c1 == c2
	(Set c1 _) == (Set c2 _)                           = c1 == c2
	(Container cl1 t1 c1 _) == (Container cl2 t2 c2 _) = cl1 == cl2 && t1 == t2 && c1 == c2
	(Element e1 _) == (Element e2 _)                   = e1 == e2
	_ == _                                             = False

data ASN1Rules = ASN1Rules
	{ checkLength :: ASN1Length -> Either ASN1Err ()
	}

-- | Basic Encoding Rules (BER)
rulesBER :: ASN1Rules
rulesBER = ASN1Rules (\_ -> return ())

-- | Distinguished Encoding Rules (DER)
rulesDER :: ASN1Rules
rulesDER = ASN1Rules checklen
	where
		checklen LenIndefinite = Left $ ASN1PolicyFailed "DER" "indefinite length not allowed"
		checklen (LenShort _)  = Right ()
		checklen (LenLong n i)
			| n == 1 && i < 0x80  = Left $ ASN1PolicyFailed "DER" "long length should be a short length"
			| n == 1 && i >= 0x80 = Right ()
			| otherwise           = if i >= 2^((n-1)*8) && i < 2^(n*8)
				then Right ()
				else Left $ ASN1PolicyFailed "DER" "long length is not shortest"

-- | pass logical chunks of events to a callback
decodeASN1Chunk :: ASN1Rules
                -> (ASN1Element -> [ASN1Event] -> Either ASN1Err a)
                -> (ASN1StructType -> [a] -> [ASN1Event] -> Either ASN1Err a)
                -> [ASN1Event]
                -> Either ASN1Err [a]
decodeASN1Chunk rules primCB structCB evs = loop evs
	where
		loop [] = return []
		loop (hdr@(Header ahdr@(ASN1Header _ _ True len)) : ConstructionBegin : xs) = do
			() <- (checkLength rules) len
			let (l, xs') = splitAtConstructionEnd 0 xs
			children <- loop l
			let localEvs = hdr : ConstructionBegin : (l ++ [ConstructionEnd])
			case structCB (getStructType ahdr) children localEvs of
				Left err -> throwError err
				Right r  -> liftM (r :) (loop xs')
		loop (hdr@(Header ahdr@(ASN1Header _ _ False len)) : p@(Primitive prim) : xs) = do
			() <- (checkLength rules) len
			o <- decodePrimitive ahdr prim
			r <- primCB o (hdr:p:[])
			liftM (r :) (loop xs)
		loop (Header _ : _) = error "mismatch event type after header"
		loop _              = error "events not starting on a header boundary"

		splitAtConstructionEnd :: Int -> [ASN1Event] -> ([ASN1Event], [ASN1Event])
		splitAtConstructionEnd _ [] = error "partial construction" -- Partial result
		splitAtConstructionEnd 0 (ConstructionEnd:xs) = ([], xs)
		splitAtConstructionEnd i (x:xs) =
			let newi = case x of
				ConstructionBegin -> i+1
				ConstructionEnd   -> i-1
				_                 -> i in
			let (ys, zs) = splitAtConstructionEnd newi xs in (x:ys,zs)

		getStructType :: ASN1Header -> ASN1StructType
		getStructType (ASN1Header Universal 0x10 _ _) = TypeSequence
		getStructType (ASN1Header Universal 0x11 _ _) = TypeSet
		getStructType (ASN1Header c t _ _)            = TypeContainer c t

-- | decode ASN1Events into a list of ASN1Tree.
-- each tree is associated with which events lead to the tree,
-- the decoding is parametrized by the rules
decodeASN1WithEvents :: ASN1Rules -> [ASN1Event] -> Either ASN1Err [ASN1Tree [ASN1Event]]
decodeASN1WithEvents rules = decodeASN1Chunk rules primCB structCB
	where
		primCB element evs        = Right $ Element element evs
		structCB sty children evs = Right $ case sty of
			TypeSequence      -> Sequence children evs
			TypeSet           -> Set children evs
			TypeContainer c t -> Container c t children evs

-- | decode ASN1 bytes to ASN1tree
decodeFromBytes :: ([ASN1Event] -> Either ASN1Err b)
                -> L.ByteString
                -> Either ASN1Err b
decodeFromBytes dec lbs = fromBytes lbs >>= dec

-- | decode ASN1Events into a list of ASN1Tree.
-- the decoding is parametrized by the rules
decodeASN1 :: ASN1Rules -> [ASN1Event] -> Either ASN1Err [ASN1Tree ()]
decodeASN1 rules = decodeASN1Chunk rules primCB structCB
	where
		primCB element _        = Right $ Element element ()
		structCB sty children _ = Right $ case sty of
			TypeSequence      -> Sequence children ()
			TypeSet           -> Set children ()
			TypeContainer c t -> Container c t children ()

-- | encode ASN1Trees into a list of ASN1Events
-- the encoding is parametrized by the rules
encodeASN1 :: ASN1Rules -> [ASN1Tree a] -> [ASN1Event]
encodeASN1 rules trees = concatMap (snd . encodeTree rules) trees

-- | encode ASN1Tree to bytes using the enc function to generate events
encodeToBytes :: ([ASN1Tree a] -> [ASN1Event]) -> [ASN1Tree a] -> L.ByteString
encodeToBytes enc trees = toBytes $ enc trees

decodePrimitive :: ASN1Header -> ByteString -> Either ASN1Err ASN1Element
decodePrimitive (ASN1Header Universal 0x1 _ _) p   = Boolean <$> getBoolean False p
decodePrimitive (ASN1Header Universal 0x2 _ _) p   = IntVal <$> getInteger p
decodePrimitive (ASN1Header Universal 0x3 _ _) p   = getBitString p >>= \(x, b) -> return $ BitString x b
decodePrimitive (ASN1Header Universal 0x4 _ _) p   = getString (OctetString) p
decodePrimitive (ASN1Header Universal 0x5 _ _) p   = getNull p >> return Null
decodePrimitive (ASN1Header Universal 0x6 _ _) p   = OID <$> getOID p
decodePrimitive (ASN1Header Universal 0x7 _ _) _   = Left $ ASN1NotImplemented "Object Descriptor"
decodePrimitive (ASN1Header Universal 0x8 _ _) _   = Left $ ASN1NotImplemented "External"
decodePrimitive (ASN1Header Universal 0x9 _ _) _   = Left $ ASN1NotImplemented "real"
decodePrimitive (ASN1Header Universal 0xa _ _) _   = Left $ ASN1NotImplemented "enumerated"
decodePrimitive (ASN1Header Universal 0xb _ _) _   = Left $ ASN1NotImplemented "EMBEDDED PDV"
decodePrimitive (ASN1Header Universal 0xc _ _) p   = getString (UTF8String . decodeUtf8) p
decodePrimitive (ASN1Header Universal 0xd _ _) _   = Left $ ASN1NotImplemented "RELATIVE-OID"
decodePrimitive (ASN1Header Universal 0x10 _ _) _  = Left $ ASN1Misc "sequence not a primitive"
decodePrimitive (ASN1Header Universal 0x11 _ _) _  = Left $ ASN1Misc "set not a primitive"
decodePrimitive (ASN1Header Universal 0x12 _ _) p  = getString NumericString p
decodePrimitive (ASN1Header Universal 0x13 _ _) p  = getString (PrintableString . decodeASCII) p
decodePrimitive (ASN1Header Universal 0x14 _ _) p  = getString (T61String . decodeASCII) p
decodePrimitive (ASN1Header Universal 0x15 _ _) p  = getString VideoTexString p
decodePrimitive (ASN1Header Universal 0x16 _ _) p  = getString (IA5String . decodeASCII) p
decodePrimitive (ASN1Header Universal 0x17 _ _) p  = UTCTime <$> getUTCTime p
decodePrimitive (ASN1Header Universal 0x18 _ _) p  = GeneralizedTime <$> getGeneralizedTime p
decodePrimitive (ASN1Header Universal 0x19 _ _) p  = getString GraphicString p
decodePrimitive (ASN1Header Universal 0x1a _ _) p  = getString VisibleString p
decodePrimitive (ASN1Header Universal 0x1b _ _) p  = getString GeneralString p
decodePrimitive (ASN1Header Universal 0x1c _ _) p  = getString (UniversalString . decodeUtf32BE) p
decodePrimitive (ASN1Header Universal 0x1d _ _) p  = getString CharacterString p
decodePrimitive (ASN1Header Universal 0x1e _ _) p  = getString (BMPString . decodeUCS2BE) p
decodePrimitive (ASN1Header tc        tag  _ _) p  = Right $ Other tc tag p

encodePrimitive :: ASN1Rules -> ASN1Element -> (Int, [ASN1Event])
encodePrimitive _ a =
	let b = encodeData a in
	let blen = B.length b in
	let len = makeLength blen in
	let hdr = encodeHeader False len a in
	(B.length (putHeader hdr) + blen, [Header hdr, Primitive b])
	where
		makeLength len
			| len < 0x80 = LenShort len
			| otherwise  = LenLong (nbBytes len) len
		nbBytes nb = if nb > 255 then 1 + nbBytes (nb `div` 256) else 1

		encodeData :: ASN1Element -> ByteString
		encodeData (Boolean b)         = B.singleton (if b then 0xff else 0)
		encodeData (IntVal i)          = putInteger i
		encodeData (BitString i bits)  = putBitString i bits
		encodeData (OctetString b)     = putString b
		encodeData Null                = B.empty
		encodeData (OID oid)           = putOID oid
		encodeData (Real _)            = B.empty -- not implemented
		encodeData Enumerated          = B.empty -- not implemented
		encodeData (UTF8String b)      = putString $ encodeUtf8 b
		encodeData (NumericString b)   = putString b
		encodeData (PrintableString b) = putString $ encodeUtf8 b
		encodeData (T61String b)       = putString $ encodeUtf8 b
		encodeData (VideoTexString b)  = putString b
		encodeData (IA5String b)       = putString $ encodeUtf8 b
		encodeData (UTCTime t)         = putUTCTime t
		encodeData (GeneralizedTime t) = putGeneralizedTime t
		encodeData (GraphicString b)   = putString b
		encodeData (VisibleString b)   = putString b
		encodeData (GeneralString b)   = putString b
		encodeData (UniversalString b) = putString $ encodeUtf32BE b
		encodeData (CharacterString b) = putString b
		encodeData (BMPString b)       = putString $ encodeUCS2BE b
		encodeData (Other _ _ b)       = b

		encodeHeader :: Bool -> ASN1Length -> ASN1Element -> ASN1Header
		encodeHeader pc len (Boolean _)         = ASN1Header Universal 0x1 pc len
		encodeHeader pc len (IntVal _)          = ASN1Header Universal 0x2 pc len
		encodeHeader pc len (BitString _ _)     = ASN1Header Universal 0x3 pc len
		encodeHeader pc len (OctetString _)     = ASN1Header Universal 0x4 pc len
		encodeHeader pc len Null                = ASN1Header Universal 0x5 pc len
		encodeHeader pc len (OID _)             = ASN1Header Universal 0x6 pc len
		encodeHeader pc len (Real _)            = ASN1Header Universal 0x9 pc len
		encodeHeader pc len Enumerated          = ASN1Header Universal 0xa pc len
		encodeHeader pc len (UTF8String _)      = ASN1Header Universal 0xc pc len
		encodeHeader pc len (NumericString _)   = ASN1Header Universal 0x12 pc len
		encodeHeader pc len (PrintableString _) = ASN1Header Universal 0x13 pc len
		encodeHeader pc len (T61String _)       = ASN1Header Universal 0x14 pc len
		encodeHeader pc len (VideoTexString _)  = ASN1Header Universal 0x15 pc len
		encodeHeader pc len (IA5String _)       = ASN1Header Universal 0x16 pc len
		encodeHeader pc len (UTCTime _)         = ASN1Header Universal 0x17 pc len
		encodeHeader pc len (GeneralizedTime _) = ASN1Header Universal 0x18 pc len
		encodeHeader pc len (GraphicString _)   = ASN1Header Universal 0x19 pc len
		encodeHeader pc len (VisibleString _)   = ASN1Header Universal 0x1a pc len
		encodeHeader pc len (GeneralString _)   = ASN1Header Universal 0x1b pc len
		encodeHeader pc len (UniversalString _) = ASN1Header Universal 0x1c pc len
		encodeHeader pc len (CharacterString _) = ASN1Header Universal 0x1d pc len
		encodeHeader pc len (BMPString _)       = ASN1Header Universal 0x1e pc len
		encodeHeader pc len (Other tc tag _)    = ASN1Header tc tag pc len

encodeConstructed :: ASN1Rules -> ASN1StructType -> [ASN1Tree a] -> (Int, [ASN1Event])
encodeConstructed rules ty children =
	let echildren = map (encodeTree rules) children in
	let clen = sum $ map fst echildren in
	let cevents = concatMap snd echildren in
	-- FIXME should use the rules here to fill the length
	let h = getStructHeader (mkSmallestLength clen) ty in
	let tlen = B.length (putHeader h) + clen in
	(tlen, ([Header h,ConstructionBegin]) ++ cevents ++ [ConstructionEnd])
	where
		getStructHeader :: ASN1Length -> ASN1StructType -> ASN1Header
		getStructHeader hlen TypeSequence        = (ASN1Header Universal 0x10 True hlen)
		getStructHeader hlen TypeSet             = (ASN1Header Universal 0x11 True hlen)
		getStructHeader hlen (TypeContainer c t) = (ASN1Header c t True hlen)

encodeTree :: ASN1Rules -> ASN1Tree a -> (Int, [ASN1Event])
encodeTree rules (Sequence children _)      = encodeConstructed rules TypeSequence children
encodeTree rules (Set children _)           = encodeConstructed rules TypeSet children
encodeTree rules (Container c t children _) = encodeConstructed rules (TypeContainer c t) children
encodeTree rules (Element el _)             = encodePrimitive rules el
