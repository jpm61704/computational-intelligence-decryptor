module Percentage where

import           Control.Exception
import           Data.Ratio
import           System.Random

newtype Percentage = Percentage Double deriving (Eq, Ord)

instance Show Percentage where
  show (Percentage x) = (show of100) ++ " %"
    where of100 = x * 100

instance Random Percentage where
  randomR ((Percentage l), (Percentage h)) gen = (Percentage x, gen')
    where (x, gen') = randomR (l, h) gen
  random gen = randomR ((Percentage 0), (Percentage 1)) gen

percentile :: Percentage -> Double -> Double
percentile (Percentage x) y = x * y

percentage :: Double -> Percentage
percentage x = assert (x >= 0 && x <= 1) (Percentage x)
