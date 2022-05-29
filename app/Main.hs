module Main where

--import Lib

import qualified Control.Monad as Monad
import qualified Data.Char as Char

main :: IO ()
main = simpleMain

simpleMain :: IO ()
-- do contains sequence of IO actions
-- the last action cannot be bound (<-)
simpleMain = do
  putStrLn "Hello, what's your name?"
  -- getLine returns IO String
  -- to make it into String, binding (<-) is the only way
  -- <- can be used only in a function with IO action (f :: IO something)
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"

ioBinding :: IO ()
ioBinding = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map Char.toUpper firstName
      bigLastName = map Char.toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- do chains together IO actions into one IO action
-- Hence it can be put after else
chainIoAction :: IO ()
chainIoAction = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      chainIoAction

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

sampleReturn :: IO Int
sampleReturn = do
  -- returns encapsulate value into an IO action
  -- it doesn't terminate the program
  return ()
  return "HAHAHA"
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4

sampleWhen :: IO ()
sampleWhen = do
  c <- getChar
  Monad.when (c /= ' ') $ do
    putChar c
    sampleWhen
