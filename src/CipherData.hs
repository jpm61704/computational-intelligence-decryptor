module CipherData where

import           CWord
import           Data.Maybe
import           Decipher
import           Dictionary

stdDict :: IO Dictionary
stdDict = loadDictionary "wordlist.txt"

key :: Key
key = keyFromList $ fmap (fromJust . convertLetter) "ABCDE"

key2 :: Key
key2 = keyFromList $ fmap (fromJust . convertLetter) "A"

key3 :: Key
key3 = keyFromList $ fmap (fromJust . convertLetter) "XXXXX"

text :: EncryptedText
text = fromJust $ fromString "igopt xqupi"

bad_text :: EncryptedText
bad_text = fromJust $ fromString "igopt xqupj"

text2 :: EncryptedText
text2 = fromJust $ fromString "ifmmp xpsme"

hw :: EncryptedText
hw = fromJust $ fromString "hello world"

testDict :: Dictionary
testDict = map (fromJust . fromString) [
    "hello", "world", "tasks", "boobs"
  ]
