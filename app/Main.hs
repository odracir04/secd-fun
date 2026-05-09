module Main where

import Parser(parser)
import SECD(run, compile)
import Control.Monad (forever)

runTerm :: String -> IO ()
runTerm = print . run . parser

secdCode :: IO ()
secdCode = do
            putStr ">>> "
            line <- getLine
            print (compile (parser line) [])


main :: IO ()
main = forever $ do
  putStr ">>> "
  line <- getLine
  print (run (parser line))

-- examples
factorial :: Int -> IO ()
factorial n = runTerm ("(fix (\\fact -> (\\n -> if n == 0 then 1 else n * fact (n - 1)))) " ++ show n)

fibonacci :: Int -> IO ()
fibonacci n = runTerm ("(fix (\\fib -> (\\n -> if n == 0 then 0 else (if n - 1 == 0 then 1 else ((fib (n - 1)) + (fib (n - 2))))))) " ++ show n)

facts :: Int -> IO ()
facts 0 = factorial 0
facts n = do
            facts (n-1)
            factorial n

fibs :: Int -> IO ()
fibs 0 = fibonacci 0
fibs n = do
            fibs (n-1)
            fibonacci n 

loop :: IO ()
loop = runTerm "(fix (\\f -> (\\x -> f x))) 1"
