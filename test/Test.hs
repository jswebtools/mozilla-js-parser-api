module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Language.JavaScript.SpiderMonkey.Parser
import Data.Aeson
import qualified Data.ByteString.Lazy as BS

main = defaultMain $ testGroup "all tests"
       [ testGroup "Low-level parser tests" lowLevelParserTests
       , testGroup "AST parser tests" astParserTests]

-- | Test reading the basic building blocks of the grammar: Node,
-- Position, SourceLocation
lowLevelParserTests =
  mkTest [("Node (location specified)", "node1",
          Node "Program" $ Just $ SourceLocation Nothing (Position 5 1) (Position 5 5))
         ,("Node (location specified)", "node2", Node "Program" Nothing)] ++
  mkTest [("Position", "position", Position {line = 1, column = 2})] ++
  mkTest [("Source location (source specified)", "source-location1",
           SourceLocation {source = Just "foo.js"
                          ,start = Position 1 2
                          ,end = Position 2 5})
         ,("Source location (source not specified)", "source-location2",
           SourceLocation {source = Nothing
                          ,start = Position 1 3
                          ,end = Position 4 2})
         ]

-- | Test reading AST's
astParserTests = [
                 ]

mkTest :: (Show a, FromJSON a, Eq a) => [(String, String, a)] -> [TestTree]
mkTest = map $ \(name, file, expected) ->
               testCase name $
               do s <- BS.readFile $ "test-data/" ++ file ++ ".json"
                  case decode s of
                    Nothing     -> assertFailure "Parse failed"
                    Just actual -> assertEqual "Unexpected value"
                                   actual expected
