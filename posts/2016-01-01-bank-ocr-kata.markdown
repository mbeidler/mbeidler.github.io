---
title: The Bank OCR Kata in Haskell
tags: Haskell, Kata
---

In this post, we're going use Haskell to solve the [Bank OCR Kata](http://codingdojo.org/cgi-bin/index.pl?KataBankOCR).

If you'd like to follow along with the code you can clone the following repository:
```bash
git clone https://github.com/mbeidler/kata-bank-ocr
```

### Summary ###

---------------

To complete this kata, we must:

1. Parse human readable bank account strings.
2. Validate the
result.
3. Print results, reporting illegible and invalid accounts as such.
4. For misses and invalid accounts, work backwards to produce a set of valid candidates that could've produced the error.

### Types ###

---------------

In Haskell, the dogma is that *if it compiles, it works*.

One of the ways we labor to achieve this is to:

> Make illegal states unrepresentable. -Yaron Minsky

So, to start, we can see that this Kata gives us a clear specification of some of the types we will be working with.

We will follow the common practice of creating a `Types`{.haskell} module so that we can hide the internal implementations and export only [smart constructors](https://wiki.haskell.org/Smart_constructors).

To represent digits we will use a simple `newtype`{.haskell} wrapper around `Int`{.haskell}. This gives us strong type safety with no runtime overhead. We will simply display it as we would an `Int`{.haskell}.
```haskell
newtype Digit = Digit { unDigit :: Int } deriving (Enum, Eq, Ord)

instance Show Digit where
    show = show . unDigit
```
Note: we do not export `unDigit`{.haskell}.

The only way to create values of `Digit`{.haskell} is to use the smart constructors we provide:
```haskell
zero, one, two, three, four, five, six, seven, eight, nine :: Digit
zero  = Digit 0
one   = Digit 1
two   = Digit 2
three = Digit 3
four  = Digit 4
five  = Digit 5
six   = Digit 6
seven = Digit 7
eight = Digit 8
nine  = Digit 9
```

We will use a similar technique for `Account`{.haskell}.
```haskell
newtype Account = Account { account :: [Digit] } deriving (Eq)

instance Show Account where
    show = concatMap show . account

fromList :: [Digit] -> Maybe Account
fromList ds | length ds == 9 = Just $ Account ds
            | otherwise      = Nothing
```

We only export `fromList`{.haskell}, so it is impossible to create an `Account`{.haskell} that doesn't have exactly 9 digits.

**User Story 2** specifies a simple checksum calculation for account verification (implemented by the `isValid`{.haskell} function). We will create a separate type for `Verified`{.haskell} accounts and use the validation in our smart constructor.

```haskell
newtype Verified = Verified { verified :: Account } deriving (Eq)

instance Show Verified where
    show = show . verified

verify :: Account -> Maybe Verified
verify a | isValid a = Just $ Verified a
         | otherwise = Nothing

isValid :: Account -> Bool
isValid (Account ds) =
    (sum $ zipWith (*) [9,8..1] (map unDigit ds)) `mod` 11 == 0
```

Because we used `{-# LANGUAGE GeneralizedNewtypeDeriving #-}`{.haskell} to derive an `Enum`{.haskell} instance for `Digit`{.haskell}, we can use our smart constructors in range notation. We'll use the `Maybe`{.haskell} monad to attempt to create a verified account.

We can test this out in our [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop):
```haskell
λ> fromList [one..nine] >>= verify
Just 123456789
λ> fromList (replicate 9 one) >>= verify
Nothing
```

Success! That does it for our `Types`{.haskell} module.

### Reading Accounts ###

------------

Let's create a new `Reader`{.haskell} module to do the heavy lifting.

We need a simple association list from a human readable 9-character `String`{.haskell} to the `Digit`{.haskell} it represents.

```haskell
digitMap :: [(String, Digit)]
digitMap =
  [ (" _ | ||_|", zero)
  , ("     |  |", one)
  , (" _  _||_ ", two)
  , (" _  _| _|", three)
  , ("   |_|  |", four)
  , (" _ |_  _|", five)
  , (" _ |_ |_|", six)
  , (" _   |  |", seven)
  , (" _ |_||_|", eight)
  , (" _ |_| _|", nine) ]
```

Reading a single digit will either fail, in which case we want to return alternatives "one character miss" away, or the digit we read. We can use Haskell's `Either`{.haskell} type for this.

```haskell
type DigitR = Either [Digit] Digit
```

We'll need the following functions for finding digits one character away:

```haskell
near :: String -> [Digit]
near str = [ d | (s,d) <- digitMap, diffSum str s == 1]

diffSum :: Eq a => [a] -> [a] -> Int
diffSum xs = length . filter not . zipWith (==) xs
```

Now we can write a simple function to read a digit from a digit string. If the lookup fails it will enumerate the *1-near* neighbors and store them in the `Left`{.haskell} constructor. Otherwise, it will simply return the `Digit`{.haskell} on the `Right`{.haskell}.

```haskell
readDigit :: String -> DigitR
readDigit s = maybe (Left $ near s) Right $ lookup s digitMap
```

We can test out our implementation in our interpreter:

- Reading the keys in the association list should all succeed.
```haskell
λ> and [ (readDigit s) == (Right d) | (s,d) <- digitMap ]
True
```

- A one character miss should return a non-empty list on the `Left`{.haskell}.
```haskell
λ> readDigit ((take 8 (fst (digitMap !! 3))) ++ " ")
Left [2,3]
```

To implement **User Story 4**, we need to be able to expand our search space when we encounter an illegible or invalid account string. We'll use this type:
```haskell
data Result = Success (Either ([DigitR], [Verified]) Verified)
            | Miss [DigitR] [Verified]
```
to represent the end result. The `Success`{.haskell} constructor represents the case where all digits were legible. The successful read is either valid (the `Right`{.haskell} side) or invalid, in which case we store the reads and the list of verified accounts one character miss away. Note: to improve readability and add a bit more type safety, the `[DigitR]`{.haskell} and `[Verified]`{.haskell} fields could be collapsed into a new type called `Candidates`{.haskell}.

So, assuming we have a list of digit strings (transforming the input data to achieve this will be covered at the end), we can run our read function to produce a `[DigitR]`{.haskell}. 

Instead of enumerating near digits every time, it is better to store there *1-near* neighbors in a `Map`{.haskell}. We'll first enumerate the relationships using our `digitMap`{.haskell} in GHCi:
```haskell
λ> :m + Control.Arrow
λ> map (near *** id) digitMap
[([8],0),([7],1),([],2),([9],3),([],4),([6,9],5),([5,8],6),([1],7),([0,6,9],8),([3,5,8],9)]
```
Then we can build our map.
```haskell
kin :: Map Digit [Digit]
kin = M.fromList $
  [ (zero,  [zero, eight])
  , (one,   [one, seven])
  , (two,   [two])
  , (three, [three, nine])
  , (four,  [four])
  , (five,  [five, six, nine])
  , (six,   [five, six, eight])
  , (seven, [one, seven])
  , (eight, [zero, six, eight, nine])
  , (nine,  [three, five, eight, nine])
  ]
```
Note: we include each key in the list of neighbors as well as it simplifies the search algorithm we will use.

To expand our search space and produce the list of valid candidates, we:

1. Collect the `Left`{.haskell} and `Right`{.haskell} digits in our `[DigitR]`{.haskell}.
2. `sequence`{.haskell} them using the list monad.
3. Map our smart constructors over them using the `Maybe`{.haskell} monad.
4. Use `catMaybes`{.haskell} to collect the `Just Verified`{.haskell} accounts.

```haskell
expand :: [[Digit]] -> [Verified]
expand =  catMaybes . map ((=<<) verify . fromList) . sequence

mapEither :: (a -> c) -> (b -> c) -> [Either a b] -> [c]
mapEither f g = map (either f g)

collect :: [DigitR] -> [[Digit]]
collect = mapEither id pure
```

When we have an account string with no illegible digits, the candidate selection process is a bit different. In this case, we need to expand our space by replacing each digit (one at a time) with the list of alternatives from our `kin`{.haskell} `Map`{.haskell}. To do so succinctly, we will use a few operators from the `Lens`{.haskell} package that make working with `Map`{.haskell}s much cleaner. The core of our solution is in the following `run`{.haskell} function.
```haskell

run :: [DigitR] -> Result
run reads
     | all isRight reads =
         Success $ case accnt >>= verify of
           Just v  -> Right v
           Nothing ->
               Left (reads, concatMap expand $ map f [0..8])
     | otherwise = Miss reads $ expand space
  where
    space = collect reads
    ds = rights reads
    accnt = fromList ds
    f i = space & ix i .~ (fromJust $ kin ^.at (ds !! i))
```

We'll write a few functions for rendering our values to strings according to the Kata's specification.

```haskell
instance Show Result where
    show (Success (Right v))       = show v
    show (Success (Left (rs, vs))) = showRes rs vs
    show (Miss rs vs)              = showRes rs vs

showRes :: [DigitR] -> [Verified] -> String
showRes rs []  = report rs ++ " ILL"
showRes _  [v] = show v
showRes rs vs  = concat [report rs, " AMB ", show vs]

report :: [DigitR] -> String
report = mapEither (const '?') (head . show)
```

And now we can verify some of the sample inputs on our `run`{.haskell} function.
```haskell
λ> run (map Right (replicate 9 one))
711111111
λ> run (map Right (replicate 9 seven))
777777177
λ> run (map Right (replicate 9 eight))
888888888 AMB [888886888,888888988,888888880]
```

Finally, we need to take the account string input and transform it into a list of digits strings.

#### Paritioning Account Strings into a List of Digits ####

----------

To read account numbers we simply need to apply the necessary transformations to the input string to get a list of strings that we can map over to produce a list of reads.

So we start with an input `String`{.haskell} and:

1. Split on newline characters using Haskell's `lines`{.haskell} function. `String -> [String]`{.haskell}
2. Use the `init`{.haskell} function to discard the last line of whitespace. `[String] -> [String]`{.haskell}
3. We further parition all the strings into 3-character triples. `[String] -> [[String]]`{.haskell} To do so, we introduce the following function:
```haskell
triples (a:b:c:xs) = [a,b,c] : triples xs
triples _          = []
```
Now, if we test out our steps so far in the interpreter, we get a list of lines, where the first line is a list of the top cell rows, and so on.
```haskell
λ> map triples . init . lines $ ones
[["   ","   ","   ","   ","   ","   ","   ","   ","   "],["  |","  |","  |","  |","  |","  |","  |","  |","  |"],["  |","  |","  |","  |","  |","  |","  |","  |","  |"]]
```

4. We can see that to continue, we need a way to combine the first element of the first line's list with the first element of the second line's list, etc. Conveniently, Haskell already has a [transpose](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html#v:transpose) function that performs exactly that operation. `[[String]] -> [[String]]`{.haskell}
```haskell
λ> :m + Data.List
λ> transpose it
[["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"],["   ","  |","  |"]]
```
5. All that remains is to collapse the 3 rows of 3 characters into one 9-character string using `map concat`{.haskell}. `[[String]] -> [String]`{.haskell}

So, now we have a transformation that takes the input string and yields a list of digit strings that we can parse.
```haskell
prepare :: String -> [String]
prepare = map concat . transpose . map triples . init . lines
  where
    triples (a:b:c:xs) = [a,b,c] : triples xs
    triples _          = []
```

And we can use `prepare`{.haskell} to create a `readAccount`{.haskell} function, which together with the `Result`{.haskell} type are our only exports.

```haskell
readAccount :: String -> Result
readAccount = run . readDigits

readDigits :: String -> [DigitR]
readDigits = map readDigit . prepare
```

### Main ###

------------

Now, we just tie everything together with a simple command line application. In our **Main.hs** file we have:

```haskell
import Reader (readAccount, Result)

main :: IO ()
main = getContents >>= mapM_ (print . readAccount) . quads . lines
  where
    quads (a:b:c:d:xs) = unlines [a,b,c,d] : quads xs
    quads _            = []
```

In a real application, I'd put this into a test suite, but that feels like going overboard in this case. Instead, I just downloaded the inputs from the Kata into a **test/accounts.txt** file, and ran the app over them to verify the results.
```bash
cat test/accounts.txt | cabal run
Preprocessing executable 'kata-bank-ocr' for kata-bank-ocr-0.0.0.1...
Running kata-bank-ocr...
711111111
777777177
200800000
333393333
888888888 AMB [888886888,888888988,888888880]
555555555 AMB [559555555,555655555]
666666666 AMB [686666666,666566666]
999999999 AMB [899999999,993999999,999959999]
490067715 AMB [490867715,490067115,490067719]
123456789
000000051
490867715
```

That's all for now! I hope you enjoyed this post. Happy Hacking!