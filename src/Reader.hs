module Reader (readSaveContents, SaveData (..)) where

import Calc (decodeName)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC8
import Data.Char (digitToInt, isDigit, ord)
import Numeric (showHex)

type Addr = Int

type Size = Int

data AddrData = AddrData Addr Size

data SaveData = SaveData String Int Int deriving (Show)

-- variable   | address  (dec)  | encoding  | size      | notes
-- name       | 0x200B   8203   | BIG       | 11  BYTES | padded with 0x50 and 0x00
-- ID         | 0x2009   8201   | BIG       | 2   BYTES | ---
-- money      | 0x23DC   9180   | BIG       | 3   BYTES | ---

addrName :: AddrData
addrName = AddrData 8203 11

addrId :: AddrData
addrId = AddrData 8201 2

addrMoney :: AddrData
addrMoney = AddrData 9180 3

hexCharToInt :: Char -> Int
hexCharToInt c
  | isDigit c = digitToInt c
  | c == 'a' = 10
  | c == 'b' = 11
  | c == 'c' = 12
  | c == 'd' = 13
  | c == 'e' = 14
  | c == 'f' = 15
  | otherwise = 0

spliceBytes :: BS.ByteString -> AddrData -> BS.ByteString
spliceBytes bytes (AddrData address size) = BS.take size (BS.drop address bytes)

-- remove 0x50 (80) and 0x00 from name bytes
filterNameBytes :: [Int] -> [Int]
filterNameBytes = filter (/= 0) . filter (/= 80)

bytesToIntArray :: BS.ByteString -> [Int]
bytesToIntArray bytes =
  map (\i -> fromIntegral (bytes `BS.index` i) :: Int) [0 .. BS.length bytes - 1]

bytesToInt :: BS.ByteString -> Int
bytesToInt bytes = sum powers
  where
    unpacked = map ord (BC8.unpack bytes)
    byteString = concatMap (`showHex` "") unpacked
    byteArrRaw = map hexCharToInt byteString
    raise :: Int -> Int
    raise i = byteArrRaw !! i * 16 ^ (length byteArrRaw - i - 1)
    powers = map raise [0 .. length byteArrRaw - 1]

readSaveContents :: BS.ByteString -> SaveData
readSaveContents contents =
  SaveData name id money
  where
    nameBytes = spliceBytes contents addrName
    nameInts = bytesToIntArray nameBytes
    name = decodeName $ filterNameBytes nameInts

    idBytes = spliceBytes contents addrId
    id = bytesToInt idBytes

    moneyBytes = spliceBytes contents addrMoney
    money = bytesToInt moneyBytes
