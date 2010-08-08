import Test.QuickCheck
import qualified Test.HUnit as Unit
import Test.HUnit ((~:), (~=?))
import Text.Printf
import Data.ASN1.Raw
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

getVal = either (const (Value Application (-1) (Constructed []))) id . runGetErr getValue
putVal = runPut . putValue

prop_getput_id :: Value -> Bool
prop_getput_id v = (getVal . putVal) v == v

tests = [ ("get.put value/id", test prop_getput_id) ]

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
