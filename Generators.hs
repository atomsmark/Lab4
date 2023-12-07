module Generators where

import Test.QuickCheck

-- Define a generator for pairs of positive integers
genPair :: Gen (Int, Int)
genPair = do
  a <- suchThat arbitrary (> 0)  -- Generate a positive integer
  b <- suchThat arbitrary (\x -> x >= 2 * a)  -- Generate an integer at least twice as big as 'a'
  return (a, b)

-- Print pairs of positive integers
printPairs :: IO ()
printPairs = generate (vectorOf 10 genPair) >>= mapM_ print

-- Main function to run the printing of pairs
main :: IO ()
main = printPairs
