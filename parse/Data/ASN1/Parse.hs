-- |
-- Module      : Data.ASN1.Parse
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A parser combinator for ASN1 Stream.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
module Data.ASN1.Parse
    ( ParseASN1
    -- * run
    , runParseASN1State
    , runParseASN1
    , throwParseError
    -- * combinators
    , onNextContainer
    , onNextContainerMaybe
    , getNextContainer
    , getNextContainerMaybe
    , getNext
    , getNextMaybe
    , hasNext
    , getObject
    , getMany
    ) where

import Data.ASN1.Types
import Data.ASN1.Stream
import Control.Applicative
import Control.Arrow (first)
import Control.Monad (liftM2)
#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail
#endif

newtype ParseASN1 a = P { runP :: [ASN1] -> Either String (a, [ASN1]) }

instance Functor ParseASN1 where
    fmap f m = P (either Left (Right . first f) . runP m)
instance Applicative ParseASN1 where
    pure a = P $ \s -> Right (a, s)
    (<*>) mf ma = P $ \s ->
        case runP mf s of
            Left err      -> Left err
            Right (f, s2) ->
                case runP ma s2 of
                    Left err      -> Left err
                    Right (a, s3) -> Right (f a, s3)
instance Monad ParseASN1 where
    return a    = pure a
    (>>=) m1 m2 = P $ \s ->
        case runP m1 s of
            Left err      -> Left err
            Right (a, s2) -> runP (m2 a) s2
instance Alternative ParseASN1 where
    empty = P $ \_ -> Left "empty Alternative"
    (<|>) m1 m2 = P $ \s ->
        case runP m1 s of
            Left _        -> runP m2 s
            Right (a, s2) -> Right (a, s2)
#if MIN_VERSION_base(4,9,0)
instance MonadFail ParseASN1 where
    fail = throwParseError
#endif

get :: ParseASN1 [ASN1]
get = P $ \stream -> Right (stream, stream)

put :: [ASN1] -> ParseASN1 ()
put stream = P $ \_ -> Right ((), stream)

-- | throw a parse error
throwParseError :: String -> ParseASN1 a
throwParseError s = P $ \_ -> Left s

-- | run the parse monad over a stream and returns the result and the remaining ASN1 Stream.
runParseASN1State :: ParseASN1 a -> [ASN1] -> Either String (a,[ASN1])
runParseASN1State f s = runP f s

-- | run the parse monad over a stream and returns the result.
--
-- If there's still some asn1 object in the state after calling f,
-- an error will be raised.
runParseASN1 :: ParseASN1 a -> [ASN1] -> Either String a
runParseASN1 f s =
    case runP f s of
        Left err      -> Left err
        Right (o, []) -> Right o
        Right (_, er) -> Left ("runParseASN1: remaining state " ++ show er)

-- | get next object
getObject :: ASN1Object a => ParseASN1 a
getObject = do
    l <- get
    case fromASN1 l of
        Left err     -> throwParseError err
        Right (a,l2) -> put l2 >> return a

-- | get next element from the stream
getNext :: ParseASN1 ASN1
getNext = do
    list <- get
    case list of
        []    -> throwParseError "empty"
        (h:l) -> put l >> return h

-- | get many elements until there's nothing left
getMany :: ParseASN1 a -> ParseASN1 [a]
getMany getOne = do
    next <- hasNext
    if next
        then liftM2 (:) getOne (getMany getOne)
        else return []

-- | get next element from the stream maybe
getNextMaybe :: (ASN1 -> Maybe a) -> ParseASN1 (Maybe a)
getNextMaybe f = do
    list <- get
    case list of
        []    -> return Nothing
        (h:l) -> let r = f h
                  in do case r of
                            Nothing -> put list
                            Just _  -> put l
                        return r

-- | get next container of specified type and return all its elements
getNextContainer :: ASN1ConstructionType -> ParseASN1 [ASN1]
getNextContainer ty = do
    list <- get
    case list of
        []                    -> throwParseError "empty"
        (h:l) | h == Start ty -> do let (l1, l2) = getConstructedEnd 0 l
                                    put l2 >> return l1
              | otherwise     -> throwParseError "not an expected container"


-- | run a function of the next elements of a container of specified type
onNextContainer :: ASN1ConstructionType -> ParseASN1 a -> ParseASN1 a
onNextContainer ty f = getNextContainer ty >>= either throwParseError return . runParseASN1 f

-- | just like getNextContainer, except it doesn't throw an error if the container doesn't exists.
getNextContainerMaybe :: ASN1ConstructionType -> ParseASN1 (Maybe [ASN1])
getNextContainerMaybe ty = do
    list <- get
    case list of
        []                    -> return Nothing
        (h:l) | h == Start ty -> do let (l1, l2) = getConstructedEnd 0 l
                                    put l2 >> return (Just l1)
              | otherwise     -> return Nothing

-- | just like onNextContainer, except it doesn't throw an error if the container doesn't exists.
onNextContainerMaybe :: ASN1ConstructionType -> ParseASN1 a -> ParseASN1 (Maybe a)
onNextContainerMaybe ty f = do
    n <- getNextContainerMaybe ty
    case n of
        Just l  -> either throwParseError (return . Just) $ runParseASN1 f l
        Nothing -> return Nothing

-- | returns if there's more elements in the stream.
hasNext :: ParseASN1 Bool
hasNext = not . null <$> get
