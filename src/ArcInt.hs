module Main where

import Parser (parse, tokenize)

main :: IO ()
main = do
  example <- readFile "example.arc"
  putStrLn $ 
    case parse example of
      Right v -> show v
      Left e  -> e
