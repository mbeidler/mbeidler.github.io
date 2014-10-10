---
title: The Bank OCR Kata in Haskell - Part 2
tags: Haskell, Kata
---

Another placeholder with some sample Haskell code to test syntax highlighting.

```haskell
module BankOCR where

import Data.Char (digitToInt)
import Data.List (transpose)
import Test.Hspec

bankOCRSpec = hspec $ do
  describe "When parsing human readable account numbers" $ do
    it "should parse 000000000" $ do
      parse zeros `shouldBe` 0
    it "should parse 111111111" $ do
      parse ones `shouldBe` 111111111
    it "should parse 123456789" $ do
      parse oneToNine `shouldBe` 123456789
    it "should validate the checksum" $ do
      isValid 711111111 `shouldBe` True

zeros = unlines
  [ "                           "
  , " _  _  _  _  _  _  _  _  _ "
  , "| || || || || || || || || |"
  , "|_||_||_||_||_||_||_||_||_|" ]

ones = unlines
  [ "                           "
  , "                           "
  , "  |  |  |  |  |  |  |  |  |"
  , "  |  |  |  |  |  |  |  |  |" ]

oneToNine = unlines
  [ "                           "
  , "    _  _     _     _  _  _ "
  , "  | _| _||_||_ |_   ||_||_|"
  , "  ||_  _|  | _||_|  ||_| _|" ]


parse :: String -> Int
parse = foldl f 0 . prep
  where
    prep = toCell . concat . transpose . map triples . tail . lines
    f n cell = let r' = p' cell in r' + 10 * n

p' :: [Char] -> Int
p' " _ | ||_|" = 0
p' "     |  |" = 1
p' " _  _||_ " = 2
p' " _  _| _|" = 3
p' "   |_|  |" = 4
p' " _ |_  _|" = 5
p' "   |_ |_|" = 6
p' " _   |  |" = 7
p' " _ |_||_|" = 8
p' " _ |_| _|" = 9
p' _           = error "unknown symbol"

triples :: [a] -> [[a]]
triples (a:b:c:xs) = [a,b,c] : triples xs
triples _          = []

toCell :: [[a]] -> [[a]]
toCell (a:b:c:xs) = (a ++ (b ++ c)) : toCell xs
toCell _          = []

isValid :: Int -> Bool
isValid n = num `mod` 11 == 0
  where 
    num = sum [ i * (digitToInt d) | (i,d) <- zip [1..9] (reverse . show $ n) ]
```