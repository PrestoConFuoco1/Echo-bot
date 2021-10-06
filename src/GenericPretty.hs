{-# LANGUAGE    TypeFamilies,
                FlexibleContexts,
                FlexibleInstances,
                DataKinds,
                TypeApplications,
                ScopedTypeVariables,
                AllowAmbiguousTypes,
                UndecidableInstances,
                MultiParamTypeClasses,
                FunctionalDependencies,
                DefaultSignatures,
                TypeSynonymInstances #-}

module GenericPretty where

--import GHC.TypeLits

import GHC.Generics

import qualified Data.Text as T (Text, pack)
import qualified Data.Text.Lazy as TL (Text, pack, unpack)

import Data.Void
import Data.Aeson.Types
import Data.Aeson (encode)
import Data.Char
import Data.Text.Lazy.Encoding (decodeUtf8)

import qualified Data.Time as Time
---------


enclose s = '{' : s ++ "}"
encloseSq s = '[' : s ++ "]"
------

data OptionsL = OptionsL {
        labelModifier :: String -> String,
        consModifier :: String -> String
    }

defaultOptionsL = OptionsL {
    labelModifier = defaultModif,
    consModifier = defaultConsModif
    }

defaultModif x@('_':ys) =
    case dropWhile (/= '_') ys of
        ('_':zs) -> zs
        _ -> x
defaultModif x = x

defaultConsModif = id

defaultConsModif' (x:xs)
    | isUpper x = case dropWhile (not . isUpper) xs of
        [] -> (x:xs)
        y  -> y
    | otherwise = (x:xs)
defaultConsModif' x = x

----------------------

data LayoutUnit = LayoutUnit String LayoutValue
    deriving (Show, Eq)

data LayoutValue = LStr String | LLay String Layout | LEmpty | LJSON String
    deriving (Show, Eq)

newtype Layout = Layout [LayoutUnit]
    deriving (Show, Eq)


lconcat :: Layout -> Layout -> Layout
lconcat (Layout l) (Layout r) = Layout $ l ++ r

defaultIndent = 4
numToIndent :: Int -> String
numToIndent ind = replicate (ind * defaultIndent) ' '

defaultWidth = 80
splitToFixedWidth :: Int -> String -> [String]
splitToFixedWidth ind s =
    let width = defaultWidth - ind * defaultIndent
        res = splitToFixedWidth' width s
    in  res

splitToFixedWidth' :: Int -> String -> [String]
splitToFixedWidth' wid [] = []
splitToFixedWidth' wid s =
    let splitted = splitAt wid s
    in  case splitted of
            (pref, suf) -> pref : splitToFixedWidth' wid suf

withIndent :: Int -> String -> String
withIndent ind str = numToIndent ind ++ str

defaultPretty :: (PrettyShow a) => a -> String
--defaultPretty title x = prettyUnit 0 (LayoutUnit title $ prettyShow x)
defaultPretty x = prettyValue 0 (prettyShow x)

textPretty :: (PrettyShow a) => a -> T.Text
textPretty = T.pack . defaultPretty

prettyUnit :: Int -> LayoutUnit -> String
prettyUnit _ (LayoutUnit s (LEmpty)) = ""
prettyUnit ind (LayoutUnit s val) =
    withIndent ind $ s ++ ": " ++ prettyValue ind val

prettyValue :: Int -> LayoutValue -> String
prettyValue ind (LStr s) = s ++ "\n"
prettyValue ind (LLay typ ls) = typ ++ "\n" ++ prettyLayout (ind + 1) ls
prettyValue ind (LJSON s) = ('\n' :) $ unlines $ map (withIndent $ ind + 1) $ splitToFixedWidth ind s
prettyValue ind LEmpty    = "empty\n"

prettyLayout :: Int -> Layout -> String
prettyLayout ind (Layout ls) = concat $ map (prettyUnit ind) ls

class PrettyShow a where
    prettyShow :: a -> LayoutValue

    default prettyShow :: (Generic a, GPrettyShow (Rep a)) => a -> LayoutValue
    prettyShow = genericPrettyShow defaultOptionsL

genericPrettyShow :: (Generic a, GPrettyShow (Rep a)) => OptionsL -> a -> LayoutValue
genericPrettyShow opts = gprettyShow opts . from
--------------------------------------------------------------------

newtype StrWrap = StrWrap { unStrWrap :: String } 

instance PrettyShow StrWrap where
    prettyShow s = LStr $ unStrWrap s

instance PrettyShow Int where
    prettyShow = LStr . show

instance PrettyShow Integer where
    prettyShow = LStr . show

instance PrettyShow Double where
    prettyShow = LStr . show

instance PrettyShow T.Text where
    --prettyShow = LStr . T.unpack
    prettyShow = LStr . show

instance PrettyShow TL.Text where
    prettyShow = LStr . show


instance PrettyShow Value where
    --prettyShow val = LStr $ "JSON value"
    prettyShow val = LJSON $ TL.unpack $ decodeUtf8 $ encode val

instance PrettyShow Bool where
    prettyShow = LStr . show

instance PrettyShow Time.Day where
    prettyShow = LStr . Time.formatTime Time.defaultTimeLocale "%F"

instance PrettyShow Void where
    prettyShow _ = LStr ""



instance (PrettyShow a) => PrettyShow (Maybe a) where
    prettyShow (Just x) = prettyShow x
    prettyShow (Nothing) = LEmpty

instance (PrettyShow a) => PrettyShow [a] where
    prettyShow [] = LEmpty
    prettyShow xs = LLay "{Array}" $ Layout $ foldr f [] $ zip [0..] xs
      where f (n, x) acc = LayoutUnit (encloseSq $ show n) (prettyShow x) : acc


------------------------------------------

class GPrettyShow f where
    gprettyShow :: OptionsL -> f a -> LayoutValue

class GPrettyShowAux f where
    gprettyShowAux :: OptionsL -> f a -> Layout

class GPrettyShowIgnoreConstr f where
    gprettyShowIgnoreConstr :: OptionsL -> f a -> LayoutValue

--------------------------------------------------------------------

instance (GPrettyShow f) => GPrettyShow (D1 d f) where
    gprettyShow opts (M1 x) = gprettyShow opts x
-- Если от нас сразу требуют LayoutValue, то тип данных не нужен

instance (GPrettyShowIgnoreConstr f, GPrettyShowIgnoreConstr g) => GPrettyShow ((:+:) f g) where
    gprettyShow opts x = gprettyShowIgnoreConstr opts x

instance (GPrettyShowIgnoreConstr f, GPrettyShowIgnoreConstr g) => GPrettyShowIgnoreConstr ((:+:) f g) where
    gprettyShowIgnoreConstr opts (L1 x) = gprettyShowIgnoreConstr opts x
    gprettyShowIgnoreConstr opts (R1 x) = gprettyShowIgnoreConstr opts x

instance (GPrettyShowIgnoreConstr f, Constructor c) => GPrettyShowIgnoreConstr (C1 c f) where
    --gprettyShowIgnoreConstr opts m@(M1 x) = if isEmpty x then LStr $ conName m else gprettyShowIgnoreConstr opts x
    gprettyShowIgnoreConstr opts m@(M1 x) = gprettyShowIgnoreConstr opts x

instance (GPrettyShow f) => GPrettyShowIgnoreConstr (S1 c f) where
    gprettyShowIgnoreConstr opts (M1 x) = gprettyShow opts x


instance (Constructor c, GPrettyShowAux f) => GPrettyShow (C1 c f) where
    gprettyShow opts m@(M1 x) = LLay (enclose $ consModifier opts $ conName m) $ gprettyShowAux opts x

instance (PrettyShow c) => GPrettyShow (Rec0 c) where
    gprettyShow opts (K1 x) = prettyShow x

--------------------------------------------------------------------

instance (GPrettyShowAux f, GPrettyShowAux g) => GPrettyShowAux ((:*:) f g) where
    gprettyShowAux opts (x :*: y) = gprettyShowAux opts x `lconcat` gprettyShowAux opts y

instance (Selector s, GPrettyShow f) => GPrettyShowAux (S1 s f) where
    gprettyShowAux opts s@(M1 x) = Layout [LayoutUnit (labelModifier opts $ selName s) (gprettyShow opts x)]

--------------------------------------------------------------------
{-
correctMaybes :: (Generic a, CorrectMaybes (Rep a)) => a -> Bool
correctMaybes x = case gCorrectMaybes . from $ x
    of Nothing -> True
       Just x  -> x

class CorrectMaybes f where
    gCorrectMaybes :: f a -> Maybe Bool

instance (CorrectMaybes f) => CorrectMaybes (D1 d f) where
    gCorrectMaybes (M1 x) = gCorrectMaybes x

instance (CorrectMaybes f) => CorrectMaybes (C1 d f) where
    gCorrectMaybes (M1 x) = gCorrectMaybes x

instance (CorrectMaybes f, CorrectMaybes g) => CorrectMaybes ((:*:) f g) where
    gCorrectMaybes (x :*: y) = gCorrectMaybes x `q` gCorrectMaybes y
      where q Nothing x = x
            q x Nothing = x
            q (Just x) (Just y) = Just $ x || y

instance (CorrectMaybes f) => CorrectMaybes (S1 d f) where
    gCorrectMaybes (M1 x) = gCorrectMaybes x

instance (CorrectMaybes' c) => CorrectMaybes (Rec0 c) where
    gCorrectMaybes (K1 x) = correctMaybes' x


--instance CorrectMaybes1 
class CorrectMaybes' a where
    correctMaybes' :: a -> Maybe Bool

   -- default correctMaybes' :: (Generic a, CorrectMaybes1 (Rep a)) => a -> Bool
    default correctMaybes' :: a -> Maybe Bool
    --correctMaybes' = gCorrectMaybes1 . from
    correctMaybes' = const Nothing

instance CorrectMaybes' Int
instance CorrectMaybes' T.Text
instance CorrectMaybes' TL.Text

instance CorrectMaybes' (Maybe a) where
    correctMaybes' = gCorrectMaybes1 . from


class CorrectMaybes1 f where
    gCorrectMaybes1 :: f a -> Maybe Bool
-- {

instance (CorrectMaybes1 f, Datatype d) => CorrectMaybes1 (D1 d f) where
    gCorrectMaybes1 z@(M1 x) =
        if datatypeName z == "Maybe"
        then gCorrectMaybes1 x
        else Nothing

instance (Constructor d) => CorrectMaybes1 (C1 d f) where
    gCorrectMaybes1 z@(M1 x) = Just $ conName z /= "Nothing"

instance (CorrectMaybes1 f, CorrectMaybes1 g) => CorrectMaybes1 ((:+:) f g) where
    gCorrectMaybes1 (L1 x) = gCorrectMaybes1 x
    gCorrectMaybes1 (R1 x) = gCorrectMaybes1 x
-- }
class CorrectMaybes2 f where
    gCorrectMaybes2 :: f a -> Bool

instance (CorrectMaybes2 f, Datatype d) => CorrectMaybes2 (D1 d f) where
    gCorrectMaybes2 z@(M1 x) =
        if datatypeName z == "Maybe"
        then gCorrectMaybes2 x
        else True

instance (Constructor d) => CorrectMaybes2 (C1 d f) where
    gCorrectMaybes2 z@(M1 x) = conName z /= "Nothing"

instance (CorrectMaybes2 f, CorrectMaybes2 g) => CorrectMaybes2 ((:+:) f g) where
    gCorrectMaybes2 (L1 x) = gCorrectMaybes2 x
    gCorrectMaybes2 (R1 x) = gCorrectMaybes2 x
-}
