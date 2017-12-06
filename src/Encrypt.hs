module Encrypt where

import           CWord
import           Decipher

{-
  decode :: Key -> EncryptedText -> CText
  decode k t = subText t additions
    where additions = case keyToCText k of
                        k' -> mconcat $ repeat k' -}


encode :: Key -> CText -> EncryptedText
encode k t = addText t additions
  where additions = case keyToCText k of
                      k' -> mconcat $ repeat k'
