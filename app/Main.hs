module Main where

import           CipherData
import           CWord
import           Decipher
import           Dictionary
import           System.Environment
import           System.Random

parseArgs :: IO (Int, Double, Double)
parseArgs = do
  strs <- getArgs
  let pop = read $ strs !! 0
  let mut = read $ strs !! 1
  let rep = read $ strs !! 2
  return (pop, mut, rep)

main :: IO ()
main = do
  dict <- stdDict
  gen <- newStdGen
  let e_dict = testDict2
  putStrLn "beggining evolution"
  args@(p,m,r) <- parseArgs
  putStrLn $ "\tpop_size:\t" ++ show p
  putStrLn $ "\tmutate_rate:\t" ++ show m
  putStrLn $ "\treproduction:\t" ++ show r
  key <- decipher2 gen dict e_dict 1 args text2
  print key
  return ()
