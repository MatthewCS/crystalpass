module Calc (calculatePassword, decodeName) where

import Data.Char (chr, ord)
import Text.Printf

-- 1 byte unsigned integer value
maxUInt1 :: Int
maxUInt1 = 256

-- 2 byte unsigned integer value
maxUInt2 :: Int
maxUInt2 = 65535

-- Pokemon Crystal does not use the ASCII table:
--   ASCII   : A = 0x41 (65)
--   CRYSTAL : A = 0x80 (128)
-- We need to shift ASCII values to make them
-- correspond with Crystal's text values
textShift :: Int
textShift = 128 - ord 'A'

-- The name value only comes from the frist
-- 5 characters in a name
calcNameValue :: String -> Int
calcNameValue name = sum [ord c + textShift | c <- first5]
  where
    first5 = take 5 name

-- ID must be less than or equal to maxUInt2.
-- If ID is greater, then we subtract it by money
--   until ID < maxUInt2.
calcIdValue :: Int -> Int -> Int
calcIdValue id money
  | id > maxUInt2 =
      calcIdValue (id - money) money
  | otherwise =
      id `div` maxUInt1 + id `mod` maxUInt1

calcMoneyValue :: Int -> Int
calcMoneyValue money =
  moneyTrunc `div` maxUInt1 + moneyTrunc `mod` maxUInt1
  where
    moneyTrunc = money `mod` maxUInt2

calculatePassword :: String -> Int -> Int -> String
calculatePassword name id money =
  printf "%05d" (nameValue + idValue + moneyValue)
  where
    nameValue = calcNameValue name
    idValue = calcIdValue id money
    moneyValue = calcMoneyValue money

-- Decode a name from array of byte values
-- Decodes from Crystal's text table to ASCII
decodeName :: [Int] -> String
decodeName nameBytes = filter (/= '\0') namePadded
  where
    decode :: Int -> Char
    decode x =
      chr (x - textShift)
    namePadded = map decode nameBytes
