{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances #-}
module Stuff (
    trace,
    echo,
    Timeout,
    safeHead,
    emptyToNothing,
    findWithDefault,
    showT,
    showTL,
    withEither,
    withMaybe
) where


import Debug.Trace (trace)
import Data.Char
import Text.Printf
import Data.List (unfoldr)
import qualified Data.Map as M (Map, findWithDefault)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

echo :: Show a => a -> a
echo x = show x `trace` x

--------------------------------------------

type Timeout = Integer

--------------------------------------------


safeHead (x:xs) = Just x
safeHead _ = Nothing


emptyToNothing :: Maybe T.Text -> Maybe T.Text
emptyToNothing (Just "") = Nothing
emptyToNothing x = x


findWithDefault :: Ord k => a -> Maybe k -> M.Map k a -> a
findWithDefault x Nothing _ = x
findWithDefault x (Just k) m = M.findWithDefault x k m

showT :: Show a => a -> T.Text
showT = T.pack . show


showTL :: Show a => a -> TL.Text
showTL = TL.pack . show



withEither :: Either a b -> (a -> c) -> (b -> c) -> c
withEither e left right = either left right e

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe m nothing just = maybe nothing just m
