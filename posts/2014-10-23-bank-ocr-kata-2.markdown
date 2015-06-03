---
title: The Bank OCR Kata in Haskell - Part 2
tags: Haskell, Kata
---

In [Part 1](http://www.typechecked.com/posts/2014-10-22-bank-ocr-kata-1.html), we implemented **User Story 1** from the [Bank OCR Kata](http://codingdojo.org/cgi-bin/index.pl?KataBankOCR). **User Story 2** specifies a simple checksum calculation to validate account numbers.

*Starting Point*
```bash
git clone https://github.com/mbeidler/BankOCR.git
git checkout -f part-1
```

### User Story 2 ###

--------------------

To start, we will add a test to our spec:

```haskell
bankOCRSpec = hspec $ do
  describe "When parsing human readable account numbers" $ do
    ...
    it "should validate the checksum" $ do
      isValid 711111111 `shouldBe` True
      isValid 111111117 `shouldBe` False
```

*The Calculation*
```python
account number:  3  4  5  8  8  2  8  6  5
position names:  d9 d8 d7 d6 d5 d4 d3 d2 d1

checksum calculation:
(d1+2*d2+3*d3 +..+9*d9) mod 11 = 0
```

We can `zip`{.haskell} the list of digit positions 1-9 with the digits in those positions.
```haskell
zip [1..9] (reverse . show $ n) :: [(Int, Char)]
```
Sum their products and check whether it is divisible by 11. 

The computation can be expressed very succintly using a list comprehension.

```haskell
isValid :: Int -> Bool
isValid n = num `mod` 11 == 0
  where 
    num = sum [ i * (digitToInt d) | (i,d) <- zip [1..9] (reverse . show $ n) ]
```

*Note*: `digitToInt`{.haskell} is imported from `Data.Char`{.haskell}.

Let's run our tests.
```bash
*BankOCR> :reload
[1 of 1] Compiling BankOCR          ( BankOCR.hs, interpreted )
Ok, modules loaded: BankOCR.
*BankOCR> bankOCRSpec

When parsing human readable account numbers
  - should parse 000000000
  - should parse 111111111
  - should parse 123456789
  - should validate the checksum

Finished in 0.0007 seconds
4 examples, 0 failures
*BankOCR> 
```

**Done!**

*Source*
```bash
git clone https://github.com/mbeidler/BankOCR.git
git checkout -f part-2
```

