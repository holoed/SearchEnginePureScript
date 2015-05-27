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
