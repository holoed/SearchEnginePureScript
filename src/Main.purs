module Main where

import Data.Tuple
import Data.String (toCharArray)
import Data.Map      (toList)
import SearchEngine
import Test.Spec (describe, pending, it)
import Test.Spec.Node
import Test.Spec.Assertions
import Test.Spec.Reporter.Console

main = runNode [consoleReporter] do

  describe "numLines tests" $ do

    it "should return first line with index" $
      numLines ["Hello World"] `shouldEqual` [Tuple "Hello World" 0]

    it "should return two lines with index" $
      numLines ["Hello", "World"] `shouldEqual` [Tuple "Hello" 0, Tuple "World" 1]

    it "should return n lines with index" $
      numLines ["Welcome to", "Real World", "of Haskell"]
       `shouldEqual` [Tuple "Welcome to" 0, Tuple "Real World" 1, Tuple "of Haskell" 2]

  describe "cleanWords tests" $ do

    it "should return words" $
       cleanWords ["Hello", "World"] `shouldEqual` ["hello", "world"]

    it "should remove numbers" $
       cleanWords ["Welcome6"] `shouldEqual` ["welcome"]

    it "should remove non ascii chars" $
       cleanWords ["This,", "is", "an", "experiment'", "of", "great", "%"]
         `shouldEqual` ["this", "is", "an", "experiment", "of", "great"]

  describe "allNumWords tests" $ do

    it "should convert lines in to words keeping line numbers" $
       allNumWords [Tuple "Welcome to" 0, Tuple "the real" 1]
         `shouldEqual` [Tuple "welcome" (Tuple 0 0), Tuple "to" (Tuple 1 0), Tuple "the" (Tuple 0 1), Tuple "real" (Tuple 1 1)]

    it "should clean while splitting into words" $
       allNumWords [Tuple "Lo ha detto papa'" 0, Tuple "che va' bene, capito" 1]
         `shouldEqual` [Tuple "lo" (Tuple 0 0), Tuple "ha" (Tuple 1 0), Tuple "detto" (Tuple 2 0), Tuple "papa" (Tuple 3 0),
                     Tuple "che" (Tuple 0 1), Tuple "va" (Tuple 1 1), Tuple "bene" (Tuple 2 1), Tuple "capito" (Tuple 3 1)]

  describe "createTermIndex tests" $ do

    it "should create an empty index for an empty document" $
       (toList <<< createTermIndex) []
         `shouldEqual` []

    it "should create an index of one element for a single word doc" $
       (toList <<< createTermIndex) ["Hello"]
         `shouldEqual` [Tuple "hello" [Tuple 0 0]]

    it "should create an index" $ do
       (toList <<< createTermIndex) ["Hello World"]
         `shouldEqual` [Tuple "hello" [Tuple 0 0], Tuple "world" [Tuple 1 0]]

       (toList <<< createTermIndex) ["Back to the Future", "A few good man"]
         `shouldEqual`[Tuple "a" [Tuple 0 1], Tuple "back" [Tuple 0 0],
                       Tuple "few" [Tuple 1 1], Tuple "future" [Tuple 3 0],
                       Tuple "good" [Tuple 2 1], Tuple "man" [Tuple 3 1],
                       Tuple "the" [Tuple 2 0], Tuple "to" [Tuple 1 0]]

       (toList <<< createTermIndex) ["A few good men", "All the President's men"]
         `shouldEqual`[Tuple "a" [Tuple 0 0], Tuple "all" [Tuple 0 1], Tuple "few"[Tuple 1 0],
                       Tuple "good" [Tuple 2 0], Tuple "men" [Tuple 3 1, Tuple 3 0],
                       Tuple "presidents" [Tuple 2 1], Tuple "the" [Tuple 1 1]]

       (toList <<< createTermIndex) ["Back to the future", "In to the future", "Back in time"]
         `shouldEqual`[Tuple "back" [Tuple 0 2, Tuple 0 0],
                       Tuple "future" [Tuple 3 1 , Tuple 3 0], Tuple "in" [Tuple 1 2, Tuple 0 1],
                       Tuple "the" [Tuple 2 1, Tuple 2 0], Tuple "time"[Tuple 2 2],
                       Tuple "to" [Tuple 1 1, Tuple 1 0]]

  describe "search tests" $ do

    it "should find single word document" $ do
        let index = createIndex ["Hello"]
        search index "Hello" `shouldEqual` [Tuple "hello" (Tuple 0 0)]

    it "should find word in two words document" $ do
        let index = createIndex ["Hello World"]
        search index "World" `shouldEqual` [Tuple "world" (Tuple 1 0)]

    it "should find word in two lines doc" $ do
        let index = createIndex ["Hello", "World"]
        search index "World" `shouldEqual` [Tuple "world" (Tuple 0 1)]

    it "should find most likely line" $ do
        let index = createIndex [
               "Back to the future",
               "Behind enemy lines",
               "Behind the planet of the apes",
               "Planet of the apes"]
        search index "Behind enemy" `shouldEqual` [Tuple "behind" (Tuple 0 1), Tuple "enemy" (Tuple 1 1)]

  describe "Partial term index tests" $ do

    it "should find all words starting with" $ do
        let partial_term_index  = createPartialTermIndex (words "welcome to the real world of back and bad")
        searchTermsFromPartial (toCharArray "b") partial_term_index `shouldEqual` (Tuple true [toCharArray "back", toCharArray "bad"])
        searchTermsFromPartial (toCharArray "ba") partial_term_index `shouldEqual` (Tuple true [toCharArray "back", toCharArray "bad"])
        searchTermsFromPartial (toCharArray "bac") partial_term_index `shouldEqual` (Tuple true [toCharArray "back"])
        searchTermsFromPartial (toCharArray "bad") partial_term_index `shouldEqual` (Tuple true [])
        searchTermsFromPartial (toCharArray "backs") partial_term_index `shouldEqual` (Tuple false [])
        searchTermsFromPartial (toCharArray "w") partial_term_index `shouldEqual` (Tuple true [toCharArray "welcome", toCharArray "world"])
        searchTermsFromPartial (toCharArray "wel") partial_term_index `shouldEqual` (Tuple true [toCharArray "welcome"])
        searchTermsFromPartial (toCharArray "wo") partial_term_index `shouldEqual` (Tuple true [toCharArray "world"])

  describe "Partial term search" $ do

    it "should find item with partial term" $ do
        let index = createIndex ["Back to the future", "Behind emeny lines"]
        search index "fut" `shouldEqual` [Tuple "fut" (Tuple 3 0)]
        search index "back to the fut" `shouldEqual` [Tuple "back" (Tuple 0 0), Tuple "to" (Tuple 1 0), Tuple "the" (Tuple 2 0), Tuple "fut" (Tuple 3 0)]

    it "should find multiple terms with partial search" $ do
        let index  = createIndex ["Back to the future","The bad guy","The fuss is all about"]
        search index "The fu" `shouldEqual` [Tuple "the" (Tuple 2 0), Tuple "fu" (Tuple 3 0), Tuple "the" (Tuple 0 2), Tuple "fu" (Tuple 1 2)]
        search index "the fut" `shouldEqual` [Tuple "the" (Tuple 2 0), Tuple "fut" (Tuple 3 0)]
        search index "the fus" `shouldEqual` [Tuple "the" (Tuple 0 2), Tuple "fus" (Tuple 1 2)]

  describe "search Line numbers" $ do

    it "should find line numbers" $ do
        let index  = createIndex ["Back to the future","The bad guy","The fuss is all about"]
        searchLineNumbers index "the fu" `shouldEqual` [0, 2]
