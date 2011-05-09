{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck
import Test.Framework(defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2(testProperty)

import Text.Printf

import Data.ASN1.Event
import Data.ASN1.Representation
import Data.ASN1.Prim

import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as T
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Data.Enumerator (($$))

import Control.Monad
import Control.Monad.Identity
import System.IO

instance Arbitrary ASN1Class where
	arbitrary = elements [ Universal, Application, Context, Private ]


instance Arbitrary ASN1Length where
	arbitrary = do
		c <- choose (0,2) :: Gen Int
		case c of
			0 -> liftM LenShort (choose (0,0x79))
			1 -> do
				nb <- choose (0x80,0x1000)
				return $ mkSmallestLength nb
			_ -> return LenIndefinite
		where
			nbBytes nb = if nb > 255 then 1 + nbBytes (nb `div` 256) else 1

arbitraryDefiniteLength :: Gen ASN1Length
arbitraryDefiniteLength = arbitrary `suchThat` (\l -> l /= LenIndefinite)

arbitraryTag :: Gen ASN1Tag
arbitraryTag = choose(0,10000)

instance Arbitrary ASN1Header where
	arbitrary = liftM4 ASN1Header arbitrary arbitraryTag arbitrary arbitrary

arbitraryEvents :: Gen ASN1Events
arbitraryEvents = do
	hdr@(ASN1Header _ _ _ len) <- liftM4 ASN1Header arbitrary arbitraryTag (return False) arbitraryDefiniteLength
	let blen = case len of
		LenLong _ x -> x
		LenShort x  -> x
		_           -> 0
	pr <- liftM Primitive (arbitraryBSsized blen)
	return (ASN1Events [Header hdr, pr])

newtype ASN1Events = ASN1Events [ASN1Event]

instance Show ASN1Events where
	show (ASN1Events x) = show x

instance Arbitrary ASN1Events where
	arbitrary = arbitraryEvents

arbitraryOID :: Gen [Integer]
arbitraryOID = do
	i1  <- choose (0,2) :: Gen Integer
	i2  <- choose (0,39) :: Gen Integer
	ran <- choose (0,30) :: Gen Int
	l   <- replicateM ran (suchThat arbitrary (\i -> i > 0))
	return (i1:i2:l)


arbitraryBSsized :: Int -> Gen B.ByteString
arbitraryBSsized len = do
	ws <- replicateM len (choose (0, 255) :: Gen Int)
	return $ B.pack $ map fromIntegral ws

instance Arbitrary B.ByteString where
	arbitrary = do
		len <- choose (0, 529) :: Gen Int
		arbitraryBSsized len

instance Arbitrary L.ByteString where
	arbitrary = do
		len <- choose (0, 529) :: Gen Int
		ws <- replicateM len (choose (0, 255) :: Gen Int)
		return $ L.pack $ map fromIntegral ws

instance Arbitrary T.Text where
	arbitrary = do
		len <- choose (0, 529) :: Gen Int
		ws <- replicateM len arbitrary
		return $ T.pack ws

arbitraryTime = do
	y <- choose (1951, 2050)
	m <- choose (0, 11)
	d <- choose (0, 31)
	h <- choose (0, 23)
	mi <- choose (0, 59)
	se <- choose (0, 59)
	z <- arbitrary
	return (y,m,d,h,mi,se,z)

arbitraryPrintString = do
	let printableString = (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ()+,-./:=?")
	x <- replicateM 21 (elements printableString)
	return $ x

arbitraryIA5String = do
	x <- replicateM 21 (elements $ map toEnum [0..127])
	return $ x

instance Arbitrary ASN1Element where
	arbitrary = oneof
		[ liftM Boolean arbitrary
		, liftM IntVal arbitrary
		, liftM2 BitString (choose (0,7)) arbitrary
		, liftM OctetString arbitrary
		, return Null
		, liftM OID arbitraryOID
		--, Real Double
		-- , return Enumerated
		, liftM UTF8String arbitrary
		, liftM NumericString arbitrary
		, liftM PrintableString arbitraryPrintString
		, liftM T61String arbitraryIA5String
		, liftM VideoTexString arbitrary
		, liftM IA5String arbitraryIA5String
		, liftM UTCTime arbitraryTime
		, liftM GeneralizedTime arbitraryTime
		, liftM GraphicString arbitrary
		, liftM VisibleString arbitrary
		, liftM GeneralString arbitrary
		, liftM UniversalString arbitrary
		]

arbitraryTreeList :: Arbitrary a => Gen [ASN1Tree a]
arbitraryTreeList = listOf1 (halfsize arbitrary)

halfsize :: Gen a -> Gen a
halfsize gen = sized (\n -> resize (n `div` 2) gen)

instance Arbitrary b => Arbitrary (ASN1Tree b) where
	arbitrary = do
		frequency
			[ (1, liftM2 Sequence arbitraryTreeList arbitrary)
			, (1, liftM2 Set arbitraryTreeList arbitrary)
			, (1, liftM4 Container arbitrary (arbitrary `suchThat` (\n -> n > 0 && n /= 0x10 && n /= 0x11)) arbitraryTreeList arbitrary)
			, (30, liftM2 Element arbitrary arbitrary)
			]

prop_header_marshalling_id :: ASN1Header -> Bool
prop_header_marshalling_id v = (getHeader . putHeader) v == Right v

prop_event_marshalling_id :: ASN1Events -> Bool
prop_event_marshalling_id (ASN1Events e) =
	let r = runIdentity $ E.run (E.enumList 1 e $$ E.joinI $ enumWriteReadBytes $$ EL.consume) in
	case r of
		Left _  -> False
		Right z -> e == z
	where
		enumWriteReadBytes = \f -> E.joinI (enumWriteBytes $$ (enumReadBytes f))

prop_repr_marshalling_id v = decodeASN1 rulesDER events == Right v
	where events = encodeASN1 rulesDER v

marshallingTests = testGroup "Marshalling"
	[ testProperty "Header" prop_header_marshalling_id
	, testProperty "Event"  prop_event_marshalling_id
	, testProperty "Repr" prop_repr_marshalling_id
	]

main = do defaultMain [marshallingTests]
