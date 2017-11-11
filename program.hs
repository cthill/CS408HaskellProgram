module Main where

import Data.Ord
import Data.List
import Data.Map.Strict


mean list = do
    let listSum = Prelude.foldr (+) 0
    (listSum list) / fromIntegral (length list)

median list = do
    let sorted = sort list
    let middle = div (length sorted) 2

    if length sorted `mod` 2 == 0 then
        mean ([sorted !! (middle - 1), sorted !! middle])
    else
        sorted !! middle

-- convert a list of Nums to a hash map: Num -> frequency
freqmap list = do
    let makePair item = (item, 0)
    let makePairs = Prelude.map makePair

    let increment hm k = do
        Data.Map.Strict.insert k ((findWithDefault 0 k hm) + 1) hm

    let hm = fromList(makePairs list)
    Prelude.foldl increment hm list

mode list = do
    (fst . last) (sortBy (comparing snd) (Data.Map.Strict.toList (freqmap list)))

stdDev list = do
    let avg = mean list
    let xi x = (x - avg) ^ 2
    let numerator = Prelude.foldr (+) 0 (Prelude.map xi list)
    let frac = numerator / fromIntegral(length list - 1)
    sqrt frac

main = do
    (print . mean) [1,2,3]
    (print . median) [1,3,4,5]
    (print . mode) [1,1,2,3]
    (print . stdDev) [1,1,2,3]
