import Test.QuickCheck
import qualified Test.HUnit as Unit
import Test.HUnit ((~:), (~=?))
import Text.Printf
import Data.ASN1.Raw
import Data.ASN1.DER (ASN1(..))
import qualified Data.ASN1.DER as DER
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

mkSeq l = Value Universal 0x10 (Constructed l)
mkBool t = Value Universal 0x1 (Primitive $ B.pack [ if t then 0x1 else 0x0 ])
mkInt t = Value Universal 0x2 (Primitive $ B.pack [ t ])

instance Arbitrary Value where
	arbitrary = elements [
		mkSeq [],
		mkSeq [ mkBool True, mkInt 124 ],
		mkSeq [ mkSeq [ mkBool True, mkInt 124 ], mkSeq [] ]
		]
	coarbitrary (Value _ tn _) = variant tn

instance Arbitrary ASN1 where
	arbitrary = elements (
		[ Boolean False, Boolean True, Null, EOC ] ++
		map IntVal ([0..512] ++ [10241024..10242048]) ++
		map OID [ [2,1,2], [1,38,53], [2,24,840,24042,530,530], [0,20,84,249,59342,53295392,325993252935] ]
		)


getVal = either (const (Value Application (-1) (Constructed []))) id . runGetErr getValue
putVal = runPut . putValue

prop_getput_value_id :: Value -> Bool
prop_getput_value_id v = (getVal . putVal) v == v

prop_getput_asn1_id :: ASN1 -> Bool
prop_getput_asn1_id v = (DER.decodeASN1 . DER.encodeASN1) v == Right v

tests =
	[ ("get.put value/id", test prop_getput_value_id)
	, ("get.put DER.ASN1/id", test prop_getput_asn1_id)
	]

quickCheckMain = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests


units :: [ (String, Value, [Word8]) ]
units = [
	("empty Sequence", mkSeq [], [ 0x30, 0x0 ]),
	("long Sequence", mkSeq (replicate 50 (mkBool True)), [ 0x30, 0x81, 150 ] ++ (concat $ replicate 50 [ 0x01, 0x1, 0x1 ]))
	]

utests :: [Unit.Test]
utests =
	map (\ (s, v, r) -> ("put " ++ s) ~: s ~: r ~=? (L.unpack $ putVal v)) units ++
	map (\ (s, v, r) -> ("get " ++ s) ~: s ~: v ~=? (getVal $ L.pack r)) units

main = do
	Unit.runTestTT (Unit.TestList utests)
	quickCheckMain
