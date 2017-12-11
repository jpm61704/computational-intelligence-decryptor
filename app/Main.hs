module Main where

import           CipherData
import           CWord
import           Decipher
import           Dictionary
import qualified Encrypt            as E
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

getTextToEncrypt :: IO CText
getTextToEncrypt = do
  putStrLn "Please enter the text to be encrypted. (Only letters and spaces)"
  x <- getLine
  case fromString x of
    (Just str) -> return str
    Nothing    -> getTextToEncrypt

getKey :: IO Key
getKey = do
  putStrLn "Please enter the key to encrypt with (Only letters)"
  x <- getLine
  return $ keyFromStr x


main :: IO ()
main = do
  dict <- stdDict
  gen <- newStdGen
  e_dict <- commonDict

  text <- getTextToEncrypt
  key <- getKey
  let encrypted_text = E.encode key text

  putStrLn "beggining evolution"
  args@(p,m,r) <- parseArgs
  putStrLn $ "\tpop_size:\t" ++ show p
  putStrLn $ "\tmutate_rate:\t" ++ show m
  putStrLn $ "\treproduction:\t" ++ show r
  key <- decipher gen dict e_dict (keyLength key) args encrypted_text
  print key
  return ()
