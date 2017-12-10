module Decipher where

import           Control.Exception
import           Control.Monad.Identity
import           Control.Monad.Morph
import           Control.Monad.State.Lazy
import           CWord
import           Data.List
import           Data.Maybe
import           Data.Ratio
import qualified Data.Vector              as V
import           Dictionary
import           Percentage
import           System.IO
import           System.Random

newtype Key = Key (V.Vector Letter) deriving (Eq)

instance Show Key where
  show (Key k) = concat $ fmap (show) k

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

hoistStateT :: Monad m => StateT s (State t) a -> StateT s (StateT t m) a
hoistStateT = undefined

decipher :: StdGen
         -> Dictionary
         -> Dictionary
         -> Int
         -> (Int, Percentage, Percentage)
         -> EncryptedText
         -> IO Key
decipher gen cutoff_dict evolve_dict key_len (pop_size, mutate_rate, percent_parents) text = do
  let xs = evalState (randomInitialPopulation pop_size key_len) gen
  x <- evalStateT (evalStateT (evolve (mutate_rate, percent_parents) cutoff_dict evolve_dict text) xs) gen
  return x

evolve :: (Percentage, Percentage)
       -> Dictionary
       -> Dictionary
       -> EncryptedText
       -> StateT (V.Vector Key) (StateT StdGen IO) Key
evolve config cutoff_dict evolve_dict text = do
  mapStateT (mapStateT generalize) $ nextGeneration config evolve_dict text
  ks <- get
  let (avg, ranks) = rankPopulation cutoff_dict text ks
  lift $ lift $ putStr $ show avg
  printKeys
  let maybe_done = checkSolution ks ranks
  case maybe_done of
    (Just done) -> return done
    Nothing     -> evolve config cutoff_dict evolve_dict text

evolve2 :: (Percentage, Percentage)
       -> Dictionary
       -> Dictionary
       -> EncryptedText
       -> StateT (V.Vector Key) (StateT StdGen IO) Key
evolve2 config cutoff_dict evolve_dict text = do
  mapStateT (mapStateT generalize) $ nextGeneration config evolve_dict text
  ks <- get
  let (ev_avg, ev_ranks) = rankPopulation evolve_dict text ks
  lift $ lift $ putStr $ show ev_avg
  printKeys
  let candidates = selectTopXPercent (percentage ev_avg) (ev_ranks) ks
      (avg, ranks) = rankPopulation cutoff_dict text candidates
      maybe_done = checkSolution candidates ranks
  case maybe_done of
    (Just done) -> return done
    Nothing     -> evolve config cutoff_dict evolve_dict text


printKeys :: StateT (V.Vector Key) (StateT StdGen IO) ()
printKeys = do
  ks <- get
  lift $ lift $ putStrLn $ "\tsample: " ++ (show (ks V.! 0)) ++ "\t" ++ (show (ks V.! 1)) ++ "\t" ++ (show (ks V.! 2))
  return ()


nextGeneration :: (Percentage, Percentage)
               -> Dictionary
               -> EncryptedText
               -> StateT (V.Vector Key) (State StdGen) ()
nextGeneration (m_rate, repo_rate) d text = do
  ks <- get
  let r@(avg, ranks) = rankPopulation d text ks
      parents  = selectTopXPercent repo_rate ranks ks
  put parents
  breed (length ks)
  mutate m_rate


-- this state business in this function doesnt feel right
breed :: Int -> StateT (V.Vector Key) (State StdGen) ()
breed n = do
  ps <- get
  let random_loc = state $ randomR (0, length ps - 1)
  i <- lift $ random_loc
  j <- lift $ random_loc
  let k1 = (ps V.! i)
      k2 = (ps V.! j)
  kid <- lift $ twoPointCrossOver k1 k2
  if n > 2 then breed (n - 2) else return ()
  kids <- get
  put $ mappend kid kids
  return ()

-- the threshold is the double size to be larger then in order to qualify as a candidate solution
checkSolution :: V.Vector Key -> V.Vector Double -> (Maybe Key)
checkSolution keys ranks = do
  case V.findIndex (== 1) ranks of
    (Just sol) -> keys V.!? sol
    Nothing    -> Nothing


mutate :: Percentage -> StateT (V.Vector Key) (State StdGen) ()
mutate p = do
  ks <- get
  lift $ forM ks ((chanceMutateKey p))
  return ()

chanceMutateKey :: Percentage -> Key -> State StdGen Key
chanceMutateKey p k = do
  t <- state $ random
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

twoPointCrossOver :: Key -> Key -> State StdGen (V.Vector Key)
twoPointCrossOver (Key x) (Key y) = do
  l1 <- state $ randomR (0, length x - 2)
  l2 <- state $ randomR (l1, length x - 1)
  let (x1, x2, x3) = splitAt3 l1 l2 x
      (y1, y2, y3) = splitAt3 l1 l2 y
      k1           = V.singleton $ Key (mconcat [x1, y2, x3])
      k2           = V.singleton $ Key (mconcat [y1, x2, y3])
  return $ mappend k1 k2

splitAt3 :: Int -> Int -> V.Vector a -> (V.Vector a, V.Vector a, V.Vector a)
splitAt3 l1 l2 v = (v1, v2, v3)
  where (v1, vs) = V.splitAt l1 v
        (v2, v3) = V.splitAt l2 vs


crossOver :: Key -> Key -> State StdGen (V.Vector Key)
crossOver (Key x) (Key y) = do
  l <- state $ randomR (1, length x - 1)
  let (x1, x2) = V.splitAt l x
      (y1, y2) = V.splitAt l y
      k1       = V.singleton $ Key (mappend x1 y2)
      k2       = V.singleton $ Key (mappend y1 x2)
  return $ mappend k1 k2

selectTopXPercent :: Percentage -> (V.Vector Double) -> V.Vector Key -> V.Vector Key
selectTopXPercent percent ranks keys
  | length ranks == length keys = V.ifilter (\i x -> getranki i >= cutoff) keys
  | otherwise = error "pop and ranks not same length"
  where getranki = \i -> fromJust $ ranks V.!? i
        cutoff = (sort $ V.toList ranks) !! (length ranks - (floor (percentile percent (fromIntegral (length ranks)))))

rankPopulation :: Dictionary -> EncryptedText -> V.Vector Key -> (Double, V.Vector Double)
rankPopulation d e ks = (avg, ranks)
  where ranks = fmap (evaluateKey d e) ks
        avg   = (sum ranks) / (fromIntegral (length ranks))

{-
evaluateKey' :: Dictionary -> EncryptedText -> Key -> Double
evaluateKey' d e k = dist
  where text = decode k e
        wrds = cwords text
        dist    = product $ map (maximum . wordMatchDistances d) wrds
-}

evaluateKey :: Dictionary -> EncryptedText -> Key -> Double
evaluateKey d e k = dist
  where text = decode k e
        wrds = cwords text
        s    = sum $ map (maximum . wordMatchDistances d) wrds
        dist = s / (fromIntegral (length wrds))

wordMatchDistances :: Dictionary -> CText -> [Double]
wordMatchDistances dict t = map (matchDistance t) dict

matchDistance :: CText -> CText -> Double
matchDistance x y
  | length x /= length y = 0
  | otherwise = (fromIntegral matches) / (fromIntegral len)
  where matches = foldr (\x acc -> if x then acc + 1 else acc) 0 (zipWith (==) x y)
        len = length x
