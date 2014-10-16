---
title: The Bank OCR Kata in Haskell - Part 1
tags: Haskell, Kata
---

If you've never experimented with [Haskell](http://www.haskell.org/haskellwiki/Haskell), wait no longer! Install the [Haskell Platform](http://www.haskell.org/platform/) for your OS of choice.

Today, we're going to solve the [Bank OCR Kata](http://codingdojo.org/cgi-bin/index.pl?KataBankOCR) with Haskell.

### Setting up a working environment ###

----------------------------------------

Create a working directory. We will sandbox our Haskell libraries here instead of installing them into [Cabal's](http://www.haskell.org/cabal/) global package database.

```bash
mkdir BankOCR
cd BankOCR
```

I recommend initializing an empty git repository to save your work at various points.
```bash
git init
echo ".cabal-sandbox" >> .gitignore
echo "dist" >> .gitignore
```

For the following command, select all defaults:
```bash
cabal init
cabal sandbox init
```

Let's create our source file:
```bash
echo "module BankOCR where" >> BankOCR.hs
```

You will now see a **BankOCR.cabal** file. We're going to use [Hspec](https://hspec.github.io/) to write our test cases, so we'll need to add that dependency to our cabal file. 

Open **BankOCR.cabal** in a text editor, add **hspec** to the build-depends and set our library to use our newly created source file:

```bash
...
library:
  exposed-module: BankOCR
  ...
  build-depends: base >=4.7 && <4.8, hspec
...
```

Now, let's install our dependencies and open a [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop) with all the dependencies loaded.

```bash
cabal install --dependencies-only
cabal repl
```

You should see something like this:

```bash
...
Loading package hspec-1.11.4 ... linking ... done.
[1 of 1] Compiling BankOCR          ( BankOCR.hs, interpreted )
Ok, modules loaded: BankOCR.
*BankOCR> 
```

Now, we can modify our BankOCR.hs file, run `cabal repl`{.bash} to reload the file into our interpreter and test out our changes. 


### Implementing the Kata ###

-----------------------------

As we are using hspec and [TDD](https://en.wikipedia.org/wiki/Test-driven_development), we'll write a simple spec for user story 1.

```haskell
module BankOCR where

import Test.Hspec

bankOCRSpec = hspec $ do
  describe "When parsing human readable account numbers" $ do
    it "should parse 000000000" $ do
      parse zeros `shouldBe` 0
    it "should parse 111111111" $ do
      parse ones `shouldBe` 111111111
    it "should parse 123456789" $ do
      parse oneToNine `shouldBe` 123456789

zeros = unlines
  [ " _  _  _  _  _  _  _  _  _ "
  , "| || || || || || || || || |"
  , "|_||_||_||_||_||_||_||_||_|"
  , "                           " ]

ones = unlines
  [ "                           "
  , "  |  |  |  |  |  |  |  |  |"
  , "  |  |  |  |  |  |  |  |  |"
  , "                           " ]

oneToNine = unlines
  [ "    _  _     _     _  _  _ "
  , "  | _| _||_||_ |_   ||_||_|"
  , "  ||_  _|  | _||_|  ||_| _|"
  , "                           " ]
```

We will also stub out our parser to return a [bottom](https://en.wikipedia.org/wiki/Bottom_type) type so that it compiles and we can run our tests.

```haskell
parse :: String -> Int
parse = undefined
```

Now, if we run `cabal repl`{.bash} and evaluate **bankOCRSpec** in the interpreter, our tests will fail.

```bash
*BankOCR> bankOCRSpec

When parsing human readable account numbers
  - should parse 000000000 FAILED [1]
  - should parse 111111111 FAILED [2]
  - should parse 123456789 FAILED [3]
...
```

We can start by using Haskell's pattern matching to write a function that maps a 9-character string to the integer it represents.

```haskell
p' :: String -> Int
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
```

Now, to read account numbers we simply need to apply the necessary transformations to the input string to get a list of strings that `p'`{.haskell} can **fold** over to **accumulate** the resulting number.

So we start with an input `String`{.haskell} and:

1. Split on newline characters using Haskell's `lines`{.haskell} function. `String -> [String]`{.haskell}
2. Use the `init`{.haskell} function to discard the last line of whitespace. `[String] -> [String]`{.haskell}
3. We further parition all the strings into 3-character triples. `[String] -> [[String]]`{.haskell} To do so, we introduce the following function:
```haskell
triples :: [a] -> [[a]]
triples (a:b:c:xs) = [a,b,c] : triples xs
triples _          = []
```
Now, if we test out our steps so far in the interpreter, we get a list of lines, where the first line is a list of the top cell rows, and so on.
```bash
*BankOCR> map triples . init . lines $ ones
[["   ","   ","   ","   ","   ","   ","   ","   ","   "],["  |","  |","  |","  |","  |","  |","  |","  |","  |"],["  |","  |","  |","  |","  |","  |","  |","  |","  |"]]
```

4. We can see that to continue, we need a way to combine the first element of the first line's list with the first element of the second line's list, etc. Conveniently, Haskell already has a [transpose](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html#v:transpose) function that performs exactly that operation. `[[String]] -> [[String]]`{.haskell}
```bash
*BankOCR> transpose it
[["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"]]
```
5. All that remains is to collapse the 3 rows of 3 characters into one 9-character string using `map concat`{.haskell}. `[[String]] -> [String]`{.haskell}

So, now we have a transformation that takes the input string and yields a list of digit strings that `p'`{.haskell} can parse and it's straight-forward to write a parse function using a left fold.

```haskell
parse :: String -> Int
parse = foldl f 0 . prep
  where
    prep = map concat . transpose . map triples . tail . lines
    f n digit = let r' = p' digit in r' + 10 * n
```

Now, if we run our tests in the interpreter:
```bash
*BankOCR> bankOCRSpec

When parsing human readable account numbers
  - should parse 000000000
  - should parse 111111111
  - should parse 123456789

Finished in 0.0004 seconds
3 examples, 0 failures
```

### Conclusion ###

---

It is quite satisfying that with pattern matching, function composition and higher-order functions, this Haskell implementation is only 17 lines long!