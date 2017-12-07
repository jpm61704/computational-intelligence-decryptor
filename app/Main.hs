module Main where

import           CipherData
import           CWord
import           Decipher
import           Dictionary
import           Percentage
import           System.Environment
import           System.Random

parseArgs :: IO (Int, Percentage, Percentage)
parseArgs = do
  strs <- getArgs
  let pop = read $ strs !! 0
  let mut = percentage $ read $ strs !! 1
  let rep = percentage $ read $ strs !! 2
  return (pop, mut, rep)

main :: IO ()
main = do
  dict <- stdDict
  gen <- newStdGen
  e_dict <- commonDict
  putStrLn "beggining evolution"
  args@(p,m,r) <- parseArgs
  putStrLn $ "\tpop_size:\t" ++ show p
  putStrLn $ "\tmutate_rate:\t" ++ show m
  putStrLn $ "\treproduction:\t" ++ show r
  key <- decipher gen dict e_dict 5 args longText2
  print key
  return ()
