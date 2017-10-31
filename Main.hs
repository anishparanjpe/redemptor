module Main where

import System.Environment

import Compile
import Evaluate
import Parser
import Show

main :: IO ()
main =
  do [fname] <- getArgs
     input <- readFile fname
     case parseProgram input of
       Left err -> putStrLn $ show err
       Right p -> putStrLn $ (showResults . eval . compile) p
