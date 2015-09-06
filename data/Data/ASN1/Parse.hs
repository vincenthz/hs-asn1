-- |
-- Module      : Data.ASN1.Parse
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.ASN1.Parse
        ( ParseASN1
        , runParseASN1State
        , runParseASN1
        , onNextContainer
        , onNextContainerMaybe
        , getNextContainer
        , getNextContainerMaybe
        , getNext
        , hasNext
        ) where

import Data.ASN1.Stream
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative (Applicative, (<$>))

-- | Parse ASN1 Monad
newtype ParseASN1 a = P { runP :: ErrorT String (State [ASN1]) a }
        deriving (Applicative, Functor, Monad, MonadError String)

-- | run the parse monad over a stream and returns the result and the remaining ASN1 Stream.
runParseASN1State :: ParseASN1 a -> [ASN1] -> Either String (a,[ASN1])
runParseASN1State f s =
        case runState (runErrorT (runP f)) s of
                (Left err, _) -> Left err
                (Right r, l)  -> Right (r,l)

-- | run the parse monad over a stream and returns the result.
runParseASN1 :: ParseASN1 a -> [ASN1] -> Either String a
runParseASN1 f s = either Left (Right . fst) $ runParseASN1State f s

-- | get next element from the stream
getNext :: ParseASN1 ASN1
getNext = do
        list <- P (lift get)
        case list of
                []    -> throwError "empty"
                (h:l) -> P (lift (put l)) >> return h

-- | get next container of specified type and return all its elements
getNextContainer :: ASN1ConstructionType -> ParseASN1 [ASN1]
getNextContainer ty = do
        list <- P (lift get)
        case list of
                []                    -> throwError "empty"
                (h:l) | h == Start ty -> do let (l1, l2) = getConstructedEnd 0 l
                                            P (lift $ put l2) >> return l1
                      | otherwise     -> throwError "not an expected container"


-- | run a function of the next elements of a container of specified type
onNextContainer :: ASN1ConstructionType -> ParseASN1 a -> ParseASN1 a
onNextContainer ty f = getNextContainer ty >>= either throwError return . runParseASN1 f

-- | just like getNextContainer, except it doesn't throw an error if the container doesn't exists.
getNextContainerMaybe :: ASN1ConstructionType -> ParseASN1 (Maybe [ASN1])
getNextContainerMaybe ty = do
        list <- P (lift get)
        case list of
                []                    -> return Nothing
                (h:l) | h == Start ty -> do let (l1, l2) = getConstructedEnd 0 l
                                            P (lift $ put l2) >> return (Just l1)
                      | otherwise     -> return Nothing

-- | just like onNextContainer, except it doens't throw an error if the container doesn't exists.
onNextContainerMaybe :: ASN1ConstructionType -> ParseASN1 a -> ParseASN1 (Maybe a)
onNextContainerMaybe ty f = do
        n <- getNextContainerMaybe ty
        case n of
                Just l  -> either throwError (return . Just) $ runParseASN1 f l
                Nothing -> return Nothing

-- | returns if there's more elements in the stream.
hasNext :: ParseASN1 Bool
hasNext = not . null <$> P (lift get)
