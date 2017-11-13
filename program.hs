module Main where

import Data.Ord
import Data.List
import Data.Map.Strict
import System.IO
import System.Environment

mean :: (Foldable t, Fractional a) => t a -> a
mean list = do
    let listSum = Prelude.foldr (+) 0
    (listSum list) / fromIntegral (length list)

{-
The line below this comment is the optional function type defnintion. It is considered
best practice to have a type definition. Everything in haskell, including functions
and expressions, has a type. The :: symbol is read as 'has type of'.

The character 'a' in the function type definition is called a type variable. It
is like a generic in Java.

The '(Ord a, Fractional a) =>' portion is called a class constraint. Here, we are
constraining the parameter 'a' to be both of the Fractional class and of the Ord
class. Fractional is a superclass of Floating and allows for the / operator. Ord
members can be compared using various comparison operators such as <= or >.

Everything following the class constraint is the function parameters. They are seperated
by the '->' symbol. In haskell, no distinction is made between the function parameters
and return type. The last item in the list of parameters is the function's return
type.

This function takes in a list '[a]' and returns a single value 'a'
-}
median :: (Ord a, Fractional a) => [a] -> a
median list = do
    let sorted = mergeSort list
    let middle = div (length sorted) 2

    if length sorted `mod` 2 == 0 then
        mean ([sorted !! (middle - 1), sorted !! middle])
    else
        sorted !! middle

{-
convert a list of items to a hash map that maps item to frequency
-}
freqmap :: (Ord k, Num v) => [k] -> Map k v
freqmap list = do
    let makePair item = (item, 0)
    let makePairs = Prelude.map makePair

    let increment hm k = do
        Data.Map.Strict.insert k ((findWithDefault 0 k hm) + 1) hm

    let hm = fromList(makePairs list)
    Prelude.foldl increment hm list

{-
Use freqmap and mergeSort to compute the mode of a given list
-}
mode :: Ord a => [a] -> a
mode list = do
    (fst . last) (mergeSortBy (comparing snd) (Data.Map.Strict.toList (freqmap list)))

{-
A group of functions to find the minimum or maximum value in the list.
-}
minmax :: (Ord a) => [a] -> (a -> a -> Bool) -> a
minmax list compFunction = do
    let winner x y = if compFunction x y then x else y
    if (length list) == 1 then
        (head list)
    else
        winner (head list) (minmax (tail list) compFunction)

findLargest :: (Ord a) => [a] -> a
findLargest list = minmax list (>)

findSmallest :: (Ord a) => [a] -> a
findSmallest list = minmax list (<)

{-
Compute the std. dev. of a list of Floating values.
-}
stdDev :: Floating a => [a] -> a
stdDev list = do
    let avg = mean list
    let xi x = (x - avg) ^ 2
    let numerator = Prelude.foldr (+) 0 (Prelude.map xi list)
    let frac = numerator / fromIntegral(length list - 1)
    sqrt frac

{-
The first parameter for this function, '(a -> a -> Ordering)', is defined as a function
that itself takes two parameters and returns an Ordering type.
-}
merge :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge orderFunc a [] = a
merge orderFunc [] a = a
{-
The (x:xs) arugment syntax defines a function that takes in a list and splits it
so that name x contains the fist element of the list, and xs contains the rest
of the list.
-}
merge orderFunc (headA:restOfListA) (headB:restOfListB)
    | ((orderFunc headA headB) == GT) = headB:(merge orderFunc (headA:restOfListA) restOfListB)
    | otherwise = headA:(merge orderFunc restOfListA (headB:restOfListB))

{-
Use mergesort to sort a list using a custom comparision function.
-}
mergeSortBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
mergeSortBy orderFunc [] = []
mergeSortBy orderFunc [a] = [a]
mergeSortBy orderFunc list = do
    let (left, right) = Data.List.splitAt (div (length list) 2) list
    merge orderFunc (mergeSortBy orderFunc left) (mergeSortBy orderFunc right)

{-
Use mergesort to sort a list. Defines a dummy comparison function and then calls
mergeSortBy.
-}
mergeSort :: (Ord a) => [a] -> [a]
mergeSort list = do
    let orderFunc x = x
    mergeSortBy (comparing orderFunc) list


choiceloop list = do
    --putStrLn (show (:t mean))
    putStrLn ("Your numbers are " ++ (show list))
    putStrLn "What would you like to do?"
    putStrLn "sort, mean, median, mode, min, max, stddev, quit?"
    putStr "> "
    hFlush stdout
    choice <- getLine

    {-
    A helper function, since all of our interactive commands are the same except
    the function they use, we can outsource the duplicate work here.
    -}
    let operate fn = do
        putStr "= "
        print (fn list)
        putStrLn ""
        choiceloop list

    case choice of
        "sort" -> operate mergeSort
        "mean" -> operate mean
        "median" -> operate median
        "mode" -> operate mode
        "max" -> operate findLargest
        "min" -> operate findSmallest
        "stddev" -> operate stdDev
        --"restart" -> putStrLn "" >> choiceloop inputlist
        "quit" -> putStrLn "Great job!"
        _ -> choiceloop list
    putStr ""

main = do
    args <- System.Environment.getArgs
    if length args == 0 then do
        putStrLn "Please enter some numbers separated by spaces."
        putStr "> "
        hFlush stdout
        input <- getLine
        choiceloop (Prelude.map (read::String->Float) (words input))
    else
        choiceloop (Prelude.map (read::String->Float) args)
