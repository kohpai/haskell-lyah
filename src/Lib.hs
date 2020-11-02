module Lib
  ( someFunc,
    surface,
  )
where

import Data.Map as Map

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- concrete types defined with data keyword
data Point = Point Float Float deriving (Show)

-- Circle and Rectangle are type constructors
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int
  }
  deriving (Show, Eq)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `scalarMult` (Vector l m n) = Vector (i * l) (j * m) (k * n)

-- type synonyms defined with type keyword

type PhoneNumber = String

type Name = String

type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name, number) `elem` book

-- type constructor that takes two types and produces a concrete type
type AssocList k v = [(k, v)]

-- partially applied type constructor
type IntMap = Map.Map Int

restrictedNull :: IntMap String -> Bool
restrictedNull v = Map.null v

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

-- use Left for error string and Right for result
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number map =
  case Map.lookup number map of
    Nothing -> Left $ "Locker number " ++ show number ++ " does not exist."
    Just (state, code) ->
      if state /= Taken
        then Right code
        else Left $ "Locker " ++ show number ++ " is already taken"

hasError :: Either String Code -> Bool
hasError v =
  case v of
    Left _ -> True
    Right _ -> False

-- deriving Show is necessary for displaying in ghci
data List a = Empty | Cons a (List a) deriving (Show)