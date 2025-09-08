module Main where

import Parser (parse)
--import Evaluator (evalProg,runEval)

main :: IO ()
main = do
  example <- readFile "example.arc"
  putStrLn $ 
    case parse example of
      Right v -> show v-- $ (runEval . evalProg) v
      Left e  -> e
