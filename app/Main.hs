module Main where

--import Lib

main :: IO ()
main = do
  putStrLn "Hello, what's your name?"
  -- getLine returns IO String
  -- to make it into String, <- is the only way
  -- <- can be used only in a function with IO action (f :: IO something)
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"
