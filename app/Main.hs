module Main where

--import Lib

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
