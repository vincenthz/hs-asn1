import Test.QuickCheck
import Text.Printf

import Data.ASN1.Raw
import Data.ASN1.DER (ASN1(..))
import qualified Data.ASN1.DER as DER

import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Control.Monad
import System.IO

arbitraryOID :: Gen [Integer]
arbitraryOID = do
	i1  <- choose (0,2) :: Gen Integer
	i2  <- choose (0,39) :: Gen Integer
	ran <- choose (0,30) :: Gen Int
	l   <- replicateM ran (suchThat arbitrary (\i -> i > 0))
	return (i1:i2:l)

instance Arbitrary B.ByteString where
	arbitrary = do
		len <- choose (0, 529) :: Gen Int
		ws <- replicateM len (choose (0, 255) :: Gen Int)
		return $ B.pack $ map fromIntegral ws

instance Arbitrary L.ByteString where
	arbitrary = do
		len <- choose (0, 529) :: Gen Int
		ws <- replicateM len (choose (0, 255) :: Gen Int)
		return $ L.pack $ map fromIntegral ws

instance Arbitrary TagClass where
	arbitrary = elements [ Universal, Application, Context, Private ]

arbitraryValueList = choose (0,20) >>= \len -> replicateM len (suchThat arbitrary (not . isConstructed))
	where
		isConstructed (Value _ _ (Constructed _)) = True
		isConstructed _                           = False

instance Arbitrary ValStruct where
	arbitrary = oneof
		[ liftM Primitive arbitrary
		, liftM Constructed arbitraryValueList ]

instance Arbitrary Value where
	arbitrary = liftM3 Value arbitrary (suchThat arbitrary (\i -> i > 0)) arbitrary

arbitraryTime = do
	y <- choose (1951, 2050)
	m <- choose (0, 11)
	d <- choose (0, 31)
	h <- choose (0, 23)
	mi <- choose (0, 59)
	se <- choose (0, 59)
	z <- arbitrary
	return (y,m,d,h,mi,se,z)

arbitraryListASN1 = choose (0, 20) >>= \len -> replicateM len (suchThat arbitrary (not . aList))
	where
		aList (Set _)      = True
		aList (Sequence _) = True
		aList _            = False

instance Arbitrary ASN1 where
	arbitrary = oneof
		[ return EOC
		, liftM Boolean arbitrary
		, liftM IntVal arbitrary
		, liftM2 BitString (choose (0,7)) arbitrary
		, liftM OctetString arbitrary
		, return Null
		, liftM OID arbitraryOID
		--, Real Double
		-- , return Enumerated
		, liftM UTF8String arbitrary
		, liftM Sequence arbitraryListASN1
		, liftM Set arbitraryListASN1
		, liftM NumericString arbitrary
		, liftM PrintableString arbitrary
		, liftM T61String arbitrary
		, liftM VideoTexString arbitrary
		, liftM IA5String arbitrary
		, liftM UTCTime arbitraryTime
		, liftM GeneralizedTime arbitraryTime
		, liftM GraphicString arbitrary
		, liftM VisibleString arbitrary
		, liftM GeneralString arbitrary
		, liftM UniversalString arbitrary
		]

prop_value_marshalling_id :: Value -> Bool
prop_value_marshalling_id v = (getVal . putVal) v == Right v
	where
		getVal = runGetErr getValue
		putVal = runPut . putValue

prop_asn1_marshalling_id :: ASN1 -> Bool
prop_asn1_marshalling_id v = (DER.decodeASN1 . DER.encodeASN1) v == Right v

args = Args
	{ replay     = Nothing
	, maxSuccess = 500
	, maxDiscard = 2000
	, maxSize    = 500
	}

run_test n t = putStr ("  " ++ n ++ " ... ") >> hFlush stdout >> quickCheckWith args t

main = do
	run_test "marshalling value = id" prop_value_marshalling_id
	run_test "marshalling asn1 = id" prop_asn1_marshalling_id
