module CipherData where

import           CWord
import           Data.Maybe
import           Decipher
import           Dictionary
import           Encrypt

stdDict :: IO Dictionary
stdDict = loadDictionary "wordlist.txt"

longText :: EncryptedText
longText = encode key $ fromJust $ fromString "My this I am so cool come be my friend"

longText2 :: EncryptedText
longText2 = encode key $ fromJust $ fromString "THIS IS COOL THIS IS TEXT THESE ARE WORKS AND THIS IS EVOLUTION AND INTELLIGENCE DONE"

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

testDict2 :: Dictionary
testDict2 = map (fromJust . fromString) [
    "my", "I", "am", "be"
  ]
