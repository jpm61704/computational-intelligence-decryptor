module Decipher where

import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.State.Lazy
import           CWord
import           Data.List
import           Data.Maybe
import qualified Data.Vector              as V
import           Dictionary
import           System.IO
import           System.Random

newtype Key = Key (V.Vector Letter) deriving (Show, Eq)

keyFromList :: [Letter] -> Key
keyFromList = Key . V.fromList

keyToCText :: Key -> CText
keyToCText (Key k) = fmap Letter (V.toList k)

keyToIntList :: Key -> [Int]
keyToIntList (Key xs) = map fromEnum (V.toList xs)

intListToEnum :: Int -> [Int] -> Int
intListToEnum base (x:xs)
  | x >= base = error "base is exceded"
  | len > 0   = ((x + 1) * (base ^ len)) + intListToEnum base xs
  | otherwise = x + intListToEnum base xs
  where len = length xs
intListToEnum _ [] = 0

instance Enum Key where
  fromEnum = (intListToEnum 26) . keyToIntList
  toEnum 0 = Key $ V.singleton A
  toEnum n = Key $ mappend ys (V.singleton y)
    where (xs,x)  = divMod n 26
          y = toEnum x
          (Key ys) = if xs > 0
                    then toEnum (xs - 1)
                    else Key mempty


instance Random Key where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (Key (V.fromList [A]), Key (V.fromList [Z,Z,Z,Z,Z])) g


type EncryptedText = CText

decode :: Key -> EncryptedText -> CText
decode k t = subText t additions
  where additions = case keyToCText k of
                      k' -> mconcat $ repeat k'

-----------------------------------------------------
--------------- evolutionary solution ---------------
-----------------------------------------------------




{-
decipher :: StdGen -> Dictionary -> Int -> EncryptedText -> (CText, Key)
decipher gen d key_len text = (decode key text, key)
  where key = evalState (decipher' d text (randomInitialPopulation 100 key_len)) gen
-}
hoistState :: Monad m => State s a -> StateT s m a
hoistState = StateT . (return .) . runState

decipher :: StdGen -> Dictionary -> Int -> (Int, Double, Double) -> EncryptedText -> IO Key
decipher gen d key_len (pop_size, mutate_rate, percent_parents) text = do
   let xs = evalState (randomInitialPopulation pop_size key_len) gen
   x <- evalStateT (evolve (mutate_rate, percent_parents) d text xs) gen
   return $ V.head x

evolve :: (Double, Double) -> Dictionary -> EncryptedText -> V.Vector Key -> StateT StdGen IO (V.Vector Key)
evolve config d t ks = do
  next_gen <- hoistState $ nextGeneration config d t ks
  let (avg, ranks) = rankPopulation d t next_gen
  lift $ print avg
  case checkSolution ranks next_gen of
    (Just done) -> return (V.singleton done)
    Nothing     -> evolve config d t next_gen



nextGeneration :: (Double, Double) -> Dictionary -> EncryptedText -> V.Vector Key -> State StdGen (V.Vector Key)
nextGeneration (m_rate, repo_rate) d text keys = assert (not (V.null keys)) $ do
      let r@(avg, ranks) = rankPopulation d text keys
          parents  = selectTopXPercent repo_rate ranks keys
      offspring <- hoistState $ breed (length keys) parents
      mutated <- mutate m_rate offspring
      return mutated



breed :: Int -> V.Vector Key -> State StdGen (V.Vector Key)
breed n ps = assert (length ps > 0) $ do
               let random_loc = state $ randomR (0, length ps - 1)
               i <- random_loc
               j <- random_loc
               let k1 = (ps V.! i)
                   k2 = (ps V.! j)
               kid <- crossOver k1 k2
               kids <- if n > 2 then breed (n - 2) ps else return mempty
               return $ mappend kid kids

checkSolution :: V.Vector Double -> V.Vector Key -> Maybe Key
checkSolution ranks keys = do
  x <- V.findIndex (== 1) ranks
  keys V.!? x

mutate :: Double -> V.Vector Key -> State StdGen (V.Vector Key)
mutate p ks = forM ks (chanceMutateKey p)


chanceMutateKey :: Double -> Key -> State StdGen Key
chanceMutateKey p k = do
  t <- state $ randomR (0, 1)
  if t < p
    then mutateKey k
    else return k

mutateKey :: Key -> State StdGen Key
mutateKey (Key letters) = do
  i <- state $ randomR (0, length letters - 1)
  new_letter <- state random
  let new_vec = V.update letters (V.singleton (i, new_letter))
  return $ Key new_vec



randomInitialPopulation :: Int -> Int -> State StdGen (V.Vector Key)
randomInitialPopulation n len = do
  let bounds = (keyFromList (replicate len A), keyFromList (replicate len Z))
  x  <- state $ randomR bounds
  xs <- randomInitialPopulation (n - 1) len
  return $ if n > 1 then V.cons x xs else V.singleton x


crossOver :: Key -> Key -> State StdGen (V.Vector Key)
crossOver (Key x) (Key y) = do
  l <- state $ randomR (1, length x - 1)
  let (x1, x2) = V.splitAt l x
      (y1, y2) = V.splitAt l y
      k1       = V.singleton $ Key (mappend x1 y2)
      k2       = V.singleton $ Key (mappend y1 x2)
  return $ mappend k1 k2

selectTopXPercent :: Double -> (V.Vector Double) -> V.Vector Key -> V.Vector Key
selectTopXPercent percent ranks keys
  | length ranks == length keys = V.ifilter (\i x -> getranki i >= percentile) keys
  | otherwise = error "pop and ranks not same length"
  where getranki = \i -> fromJust $ ranks V.!? i
        percentile = (sort $ V.toList ranks) !! ((length ranks) - (floor (percent * (fromIntegral (length ranks)))))

rankPopulation :: Dictionary -> EncryptedText -> V.Vector Key -> (Double, V.Vector Double)
rankPopulation d e ks = (avg, ranks)
  where ranks = fmap (evaluateKey d e) ks
        avg   = (sum ranks) / (fromIntegral (length ranks))



evaluateKey :: Dictionary -> EncryptedText -> Key -> Double
evaluateKey d e k = dist
  where text = decode k e
        wrds = cwords text
        dist = product $ map (maximum . wordMatchDistances d) wrds

wordMatchDistances :: Dictionary -> CText -> [Double]
wordMatchDistances dict t = map (matchDistance t) dict

matchDistance :: CText -> CText -> Double
matchDistance x y
  | length x /= length y = 0
  | otherwise = (fromIntegral matches) / (fromIntegral len)
  where matches = foldr (\x acc -> if x then acc + 1 else acc) 0 (zipWith (==) x y)
        len = length x
