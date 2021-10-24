{-# LANGUAGE OverloadedStrings, FlexibleInstances,
  UndecidableInstances #-}
module Stuff where

import qualified Data.Map as M (Map, findWithDefault)
import qualified Data.Text as T
import Debug.Trace (trace)

echo :: Show a => a -> a
echo x = show x `trace` x

--------------------------------------------
type Timeout = Integer

--------------------------------------------
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

emptyToNothing :: Maybe T.Text -> Maybe T.Text
emptyToNothing (Just "") = Nothing
emptyToNothing x = x

findWithDefault :: Ord k => a -> Maybe k -> M.Map k a -> a
findWithDefault x Nothing _ = x
findWithDefault x (Just k) m = M.findWithDefault x k m

showT :: Show a => a -> T.Text
showT = T.pack . show

withEither :: Either a b -> (a -> c) -> (b -> c) -> c
withEither e left right = either left right e

withMaybe :: Maybe a -> b -> (a -> b) -> b
withMaybe m nothing just = maybe nothing just m
