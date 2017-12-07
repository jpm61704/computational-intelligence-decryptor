module CipherData where

import           CWord
import qualified Data.Char  as C
import           Data.Maybe
import           Decipher
import           Dictionary
import           Encrypt

stdDict :: IO Dictionary
stdDict = loadDictionary "wordlist.txt"

lowFreqDict :: IO Dictionary
lowFreqDict = loadDictionary "low_freq_dict.txt"

commonDict :: IO Dictionary
commonDict = loadDictionary "commom_words_dict.txt"

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

long_long_text :: EncryptedText
long_long_text = encode key $ fromJust $ fromString $ filter (\x-> C.isAlpha x || ((==) x ' ')) "As absolute is by amounted repeated entirely ye returned. These ready timed enjoy might sir yet one since. Years drift never if could forty being no. On estimable dependent as suffering on my. Rank it long have sure in room what as he. Possession travelling sufficient yet our. Talked vanity looked in to. Gay perceive led believed endeavor. Rapturous no of estimable oh therefore direction up. Sons the ever not fine like eyes all sure."

testDict :: Dictionary
testDict = map (fromJust . fromString) [
    "hello", "world", "tasks", "boobs"
  ]

testDict2 :: Dictionary
testDict2 = map (fromJust . fromString) [
    "my", "I", "am", "be"
  ]
