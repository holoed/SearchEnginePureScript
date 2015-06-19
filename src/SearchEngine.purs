module SearchEngine where

import Data.Maybe.Unsafe (fromJust)
import Data.Map (Map(), fromList, lookup, insert, empty, values, keys)
import Control.Apply
import Data.Tuple
import Data.String (trim, split, toCharArray, fromCharArray, toLower, indexOf, joinWith)
import Data.Char
import Data.Array (range, length, filter, null, sort, sortBy, groupBy, drop, concatMap, concat, group)
import Data.Foldable (foldl, all)
import Data.Maybe
import Data.Function (on)
import Data.Array.Unsafe (head)

type Doc = [Line]
type Line = String
type Word = String
type LineNumber = Number
type Pos = Number
type Index = Map Word [Tuple Pos LineNumber]

data IndexedWord = IndexedWord { word:: Word, pos:: Pos, line:: LineNumber }

data PartialTermIndex = D (Map Char (Tuple [[Char]] PartialTermIndex))

both :: forall a. (a -> Boolean) -> (a -> Boolean) -> a -> Boolean
both = lift2 (&&)

lines :: String -> [Line]
lines s = trim <$> (split "\n" s)

words :: String -> [Word]
words s = trim <$> (split " " s)

numLines :: Doc -> [Tuple Line LineNumber]
numLines doc = flip zip ns doc
    where ns = range 0 (length doc)

numWords :: Line -> [Tuple Word Pos]
numWords line = flip zip ns ws
    where ns = range 0 (length ws)
          ws = cleanWords (words line)

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

allNumWords :: [Tuple Line LineNumber] -> [Tuple Word (Tuple Pos LineNumber)]
allNumWords = (>>= (\(Tuple l i) -> (<$>) (\(Tuple w p) -> Tuple w (Tuple p i)) $ numWords l))

makeLists :: [Tuple Word (Tuple Pos LineNumber)] -> [Tuple Word [Tuple Pos LineNumber]]
makeLists = (<$>) (\(Tuple w (Tuple p i)) -> Tuple w [Tuple p i])

accumulate :: [Tuple Word [Tuple Pos LineNumber]] -> [Tuple Word [Tuple Pos LineNumber]]
accumulate = foldl f []
           where f [] x = [x]
                 f ((Tuple w ls):xs) (Tuple w2 [l]) | w == w2 = (Tuple w (l:ls)):xs
                 f ((Tuple w ls):xs) (Tuple w2 [l]) | w /= w2 = (Tuple w2 [l]):((Tuple w ls):xs)

createTermIndex :: Doc -> Index
createTermIndex =  fromList <<< accumulate <<< makeLists <<< sort <<< allNumWords <<< numLines

createPartialTermIndex :: [String] -> PartialTermIndex
createPartialTermIndex = createPartialTermIndex' <<< (<$>) toCharArray
  where
    createPartialTermIndex' []  = D empty
    createPartialTermIndex' ws  =
        foldl (\(D m) (Tuple k ws') -> D $ insert k (Tuple ws' (createPartialTermIndex' ((<$>) (drop 1) ws'))) m) (D empty) <<<
        (<$>) (\ws' -> Tuple (head (head ws')) ws') <<<
        groupBy ((==) `on` head) <<<
        sortBy (compare `on` head) <<<
        filter (not <<< null) $ ws

searchTermsFromPartial :: [Char] -> PartialTermIndex -> Tuple Boolean [[Char]]
searchTermsFromPartial s d = findTerms s d
  where
    flattenValues = (<$>) (s ++) <<< concatMap fst <<< values

    findTerms [] (D d') = Tuple true (flattenValues d')
    findTerms (x:xs) (D d') =
      case lookup x d' of
            Just (Tuple _ d'') -> findTerms xs d''
            Nothing -> Tuple false (flattenValues d')

createIndex :: Doc -> Tuple Index PartialTermIndex
createIndex doc = Tuple index (createPartialTermIndex (keys index))
  where index = createTermIndex doc


getWordIndex :: Tuple Index PartialTermIndex -> String -> [Tuple Pos LineNumber]
getWordIndex (Tuple i pti) w = fromMaybe partialResults
                                    ((++partialResults) <$> lookup w i)
              where matches = searchTermsFromPartial (toCharArray w) pti
                    partialResults = if fst matches then concatMap (\m -> fromJust (lookup m i)) (fromCharArray <$> (snd matches))
                                                    else []

pos :: IndexedWord -> Pos
pos (IndexedWord { word:w, pos:p, line:l }) = p

line :: IndexedWord -> LineNumber
line (IndexedWord { word:w, pos:p, line:l }) = l

word :: IndexedWord -> Word
word (IndexedWord { word:w, pos:p, line:l }) = w

getIndexedWords :: Tuple Index PartialTermIndex -> [Word] -> [IndexedWord]
getIndexedWords i = concatMap (\w -> (<$>) (\(Tuple p l) -> IndexedWord{word:w,pos:p,line:l}) (getWordIndex i w))


groupByLine :: [IndexedWord] -> [[IndexedWord]]
groupByLine = (<$>) (sortBy(compare `on` pos)) <<<
              groupBy ((==) `on` line) <<<
              sortBy(compare `on` line)

equalTermsAndResults :: [Word] -> [IndexedWord] -> Boolean
equalTermsAndResults ws rs = (length ws) <= (length ws') && ((indexOf (joinWith "," (sort ws)) (joinWith "," (sort ws'))) > -1)
        where ws' = word <$> rs

toTuple :: IndexedWord -> Tuple Word (Tuple Pos LineNumber)
toTuple iw = Tuple (word iw) (Tuple (pos iw) (line iw))

search :: Tuple Index PartialTermIndex -> String -> [Tuple Word (Tuple Pos LineNumber)]
search i s = toTuple <$> (concat (filter predicate (groupByLine plwords)))
      where
            predicate = equalTermsAndResults cleaned_words
            cleaned_words = (cleanWords <<< words) s
            plwords = getIndexedWords i cleaned_words

searchLineNumbers :: Tuple Index PartialTermIndex -> String -> [LineNumber]
searchLineNumbers i s =  head <$> (group (sort ((snd <<< snd) <$> (search i s))))
