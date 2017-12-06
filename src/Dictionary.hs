module Dictionary where


import           CWord
import           Data.Maybe
import           System.IO

type Dictionary = [CText]

loadDictionary :: FilePath -> IO Dictionary
loadDictionary fp = do
  fh <- openFile fp ReadMode
  hSetNewlineMode fh universalNewlineMode
  contents <- hGetContents fh
  let lns = lines contents
  return $ catMaybes $ map (fromString) lns
