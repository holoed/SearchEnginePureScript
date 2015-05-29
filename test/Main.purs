module Test.Main where

import Test.Unit
import SearchEngine

main = runTest do

  test "Empty Index Search tests" do
    let index = createIndex []
    assert "Empty index with empty string" $ search index "" == []
    assert "Empty index with non-empty string" $ search index "Foo" == []

  test "One word Index Search tests" $ do
    let index = createIndex ["Hello"]
    assert "Search with empty string" $ search index "" == []
    assert "Search with non existing word" $ search index "Foo" == []
    assert "Search with existing word" $ search index "Hello" == [1]

  test "One line Index Search tests" $ do
    let index = createIndex ["Welcome to the world"]
    assert "Search with empty string" $ search index "" == []
    assert "Search with non existing word" $ search index "Foo" == []
    assert "Search with existing word" $ search index "Welcome" == [1]
    assert "Search with existing words" $ search index "welcome world" == [1]

  test "Many lines Index Search tests" $ do
    let index = createIndex ["Back to the Future", "The Future is now"]
    assert "Search with empty string" $ search index "" == []
    assert "Search with single word" $ search index "future" == [2, 1]
    assert "Search with more words" $ search index "Back future" == [1]
    assert "Search with other words" $ search index "Future now" == [2]
