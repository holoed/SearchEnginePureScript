module SearchEngine where

import Debug.Trace
--import Data.Function
--import Data.Char
--import Data.List
import Data.Maybe.Unsafe (fromJust)
import Data.Map (Map(), fromList, lookup)
import Control.Apply
--import Control.Monad
import Data.Tuple
import Data.String (trim, split, toCharArray, fromCharArray, toLower)
import Data.Char
--import Data.Sequence (fmap)
import Data.Array (range, length, filter, null, sort, sortBy, concatMap, concat)
import Data.Foldable (foldl)
import Data.Maybe

type Doc = [Line]
type Line = String
type Word = String
type LineNumber = Number
type Index = Map Word [LineNumber]
type Frequency = Number

both :: forall a. (a -> Boolean) -> (a -> Boolean) -> a -> Boolean
both = lift2 (&&)

lines :: String -> [String]
lines s = trim <$> (split "\n" s)

words :: String -> [String]
words s = trim <$> (split " " s)

numLines :: Doc -> [Tuple Line LineNumber]
numLines doc = flip zip ns doc
    where ns = range 1 (length doc)

isUpperCaseLetter :: Char -> Boolean
isUpperCaseLetter ch = toCharCode ch >= 65 && toCharCode ch <= 90

isLowerCaseLetter :: Char -> Boolean
isLowerCaseLetter ch = toCharCode ch >= 97 && toCharCode ch <= 122

isLetter :: Char -> Boolean
isLetter ch = isUpperCaseLetter ch || isLowerCaseLetter ch

isAscii :: Char -> Boolean
isAscii ch = toCharCode ch >= 0 && toCharCode ch <= 127

cleanWords :: [Word] -> [Word]
cleanWords = (<$>) toLower <<<
             (<$>) fromCharArray <<<
             filter (not <<< null) <<<
             (<$>) (filter (both isLetter isAscii)) <<<
             (<$>) toCharArray

allNumWords :: [Tuple Line LineNumber] -> [Tuple Word LineNumber]
allNumWords = (>>= (\(Tuple l i) -> (<$>) (\w -> Tuple w i) $ cleanWords $ words l))

makeLists :: [Tuple Word LineNumber] -> [Tuple Word [LineNumber]]
makeLists = (<$>) (\(Tuple w i) -> Tuple w [i])

accumulate :: [Tuple Word [LineNumber]] -> [Tuple Word [LineNumber]]
accumulate = foldl f []
           where f [] x = [x]
                 f ((Tuple w ls):xs) (Tuple w2 [l]) | w == w2 = (Tuple w (l:ls)):xs
                 f ((Tuple w ls):xs) (Tuple w2 [l]) | w /= w2 = (Tuple w2 [l]):((Tuple w ls):xs)


createIndex :: Doc -> Index
createIndex =  fromList <<< accumulate <<< makeLists <<< sort <<< allNumWords <<< numLines

frequencies :: forall a. (Eq a, Ord a) => [a] -> [Tuple a Frequency]
frequencies = sortBy (\(Tuple _ x) (Tuple _ y) -> compare y x) <<< foldl count [] <<< sort
  where
    count [] x = [Tuple x 1]
    count ((Tuple y i):xs) x | x == y = (Tuple y (i + 1)):xs
    count ((Tuple y i):xs) x          = (Tuple x 1):((Tuple y i):xs)

search :: Index -> String -> [LineNumber]
search i s = (<$>) fst (filter (\(Tuple _ f) -> f == l) (frequencies found))
  where ws = cleanWords (words s)
        l = length ws
        found = concat $ concatMap (maybe [] (\x -> [x]) <<< flip lookup i) ws
