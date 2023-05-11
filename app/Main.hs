module Main (main) where

import Calc (calculatePassword)
import qualified Data.ByteString.Lazy as BL
import Reader (SaveData (..), readSaveContents)
import System.Directory
import System.Environment
import Text.Printf

checkArgs :: [String] -> IO ()
checkArgs args =
  if length args == 1
    then putStrLn $ printf "Loading %s ..." (head args)
    else error "ERROR: One argument (filepath) must be given. Aborting ..."

checkFilepath :: String -> IO ()
checkFilepath filepath = do
  fileExists <- doesFileExist filepath
  if fileExists
    then return ()
    else error $ printf "ERROR: file \"%s\" not found. Aborting ..." filepath

passwordFromSaveData :: SaveData -> String
passwordFromSaveData (SaveData name id money) = calculatePassword name id money

printSaveData :: SaveData -> IO ()
printSaveData (SaveData name id money) = do
  putStrLn "Trainer data:"
  putStrLn $ printf "  Name:  %s" name
  putStrLn $ printf "  ID:    %d" id
  putStrLn $ printf "  Money: %d" money

main :: IO ()
main = do
  args <- getArgs
  checkArgs args
  let filepath = head args
  checkFilepath filepath
  contents <- BL.readFile filepath
  putStrLn "  File loaded!"

  let saveData = readSaveContents (BL.toStrict contents)
  let password = passwordFromSaveData saveData
  printSaveData saveData
  putStrLn $ printf "PASSWORD: *** %s ***" password
