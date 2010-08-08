module Data.ASN1.Parser (
	runParseASN1,
	pSequence
	) where

import Data.ASN1.Raw
import Data.ASN1.Prim
import Control.Monad.State
import Control.Monad.Error

newtype ParseASN1 a = P { runP :: ErrorT String (State [Value]) a }

runParseASN1 :: ParseASN1 a -> Value -> Either String a
runParseASN1 f value =
	case runState (runErrorT (runP f)) [value] of
		(Left err, _) -> Left err
		(Right r, _)  -> Right r

getNext :: ParseASN1 Value
getNext = do
	list <- P (lift get)
	case list of
		[]    -> throwError "empty"
		(h:l) -> P (lift (put l)) >> return h

pSequence f = do
	n <- getNext
	case n of
		TagSequence -> case runParseASN1 
	case getTag of
