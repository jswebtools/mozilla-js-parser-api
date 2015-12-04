{-# LANGUAGE OverloadedStrings #-}
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
-- TODO fix compilation error and uncomment
--  mkTest [
--    ("Node (location specified)", "node1",
--          Node "Program" $ Just $ SourceLocation Nothing (Position 5 1) (Position 5 5))
--         ,("Node (location specified)", "node2", Node "Program" Nothing)] ++
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
astParserTests =
  mkTest [("Test 10.1.1.1-S from test262", "test10", Program {loc = SourceLocation {source = Nothing, start = Position {line = 1, column = 0}, end = Position {line = 21, column = 22}}, body = [ExpressionStatement (SourceLocation {source = Nothing, start = Position {line = 21, column = 0}, end = Position {line = 21, column = 22}}) (CallExpression (SourceLocation {source = Nothing, start = Position {line = 21, column = 0}, end = Position {line = 21, column = 20}}) (IdentifierExpression (SourceLocation {source = Nothing, start = Position {line = 21, column = 0}, end = Position {line = 21, column = 11}}) (Identifier (SourceLocation {source = Nothing, start = Position {line = 21, column = 0}, end = Position {line = 21, column = 11}}) "runTestCase")) [IdentifierExpression (SourceLocation {source = Nothing, start = Position {line = 21, column = 12}, end = Position {line = 21, column = 20}}) (Identifier (SourceLocation {source = Nothing, start = Position {line = 21, column = 12}, end = Position {line = 21, column = 20}}) "testcase")])]})
         --,("Test 10.1.1_1 from test262", "test10.1.1_1")
         ]

mkTest :: (Show a, FromJSON a, Eq a) => [(String, String, a)] -> [TestTree]
mkTest = map $ \(name, file, expected) ->
               testCase name $
               do s <- BS.readFile $ "test-data/" ++ file ++ ".json"
                  case decode s of
                   Nothing     -> assertFailure "Parse failed"
                   Just actual -> assertEqual "Unexpected value"
                                  actual expected
