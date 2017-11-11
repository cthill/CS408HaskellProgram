module Main where

import Data.Ord
import Data.List
import Data.Map.Strict
import System.IO
import System.Environment


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

{-inputlist = do
    putStrLn "Please enter some numbers separated by spaces."
    putStr "> "
    input <- getLine
    return (Prelude.map (read::String->Float) (words input))-}

choiceloop list = do
    putStrLn ("Your numbers are " ++ (show list))
    putStrLn "What would you like to do?"
    putStrLn "mean, median, mode, stddev, quit?"
    putStr "> "
    hFlush stdout
    choice <- getLine

    let operate fn = do
        putStr "= "
        print (fn list)
        putStrLn ""
        choiceloop list

    case choice of
        "mean" -> operate mean
        "median" -> operate median
        "mode" -> operate mode
        "stddev" -> operate stdDev
        --"restart" -> putStrLn "" >> choiceloop inputlist
        "quit" -> putStrLn "Great job!"
        _ -> choiceloop list
    putStr ""

main = do
    args <- System.Environment.getArgs
    if length args == 0 then
        putStrLn "Please provide your numbers as cmd args"
    else
        choiceloop (Prelude.map (read::String->Float) args)
