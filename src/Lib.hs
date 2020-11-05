module Lib
  ( surface,
  )
where

import qualified Data.Map as Map

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
hasError (Left _) = True
hasError (Right _) = False

infixr 5 :-:

-- deriving Show is necessary for displaying in ghci
data List a = Empty | a :-: (List a) deriving (Show)

infixr 5 .++

(.++) :: List a -> List a -> List a
Empty .++ l = l
(x :-: l1) .++ l2 = x :-: (l1 .++ l2)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: Ord a => a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node root lt rt)
  | x < root = Node root (treeInsert x lt) rt
  | otherwise = Node root lt $ treeInsert x rt

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node root lt rt)
  | x == root = True
  | x < root = treeElem x lt
  | otherwise = treeElem x rt

data TrafficLight = Red | Yellow | Green

-- minimal complete definition for the typeclass, we don't define /= (it already has default implementation)
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id -- takes parameter and return same thing

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

instance YesNo (List a) where
  yesno Empty = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoExpr yesVal noVal = if yesno yesnoExpr then yesVal else noVal

level :: Tree a -> Integer
level EmptyTree = 0
level (Node x l r) = max (1 + level l) (1 + level r)

depth :: Tree a -> Integer
depth EmptyTree = 0
depth t = level t - 1

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node root left right) = Node (f root) (fmap f left) (fmap f right)

-- kind of t :: * -> (* -> *) -> *
class Tofu t where
  tofu :: j a -> t a j

-- kind of Frank :: * -> (* -> *) -> *
data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
  tofu x = Frank x

-- kind of Barry :: (* -> *) -> * -> * -> *
data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
