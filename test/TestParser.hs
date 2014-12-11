module Main where

import Language.JavaScript.SpiderMonkey.Parser
import Data.Aeson
import qualified Data.ByteString.Lazy as BS

import System.Environment( getArgs )

-- parse :: FromJSON a => BS.ByteString -> [Either (String,Value) a]
-- parse s = case eitherDecode s of
--            Left e -> error e
--            Right values -> map tryDecode values
--   where
--     tryDecode :: FromJSON a =>Value -> Either (String,Value) a
--     tryDecode v = case eitherDecode (encode v) of
--       Left x -> Left (x,v)
--       Right a -> Right a

    
main :: IO ()
main = do [fileName] <- getArgs
          s <- BS.readFile fileName
          let res = eitherDecode s :: Either String Program
          print res

