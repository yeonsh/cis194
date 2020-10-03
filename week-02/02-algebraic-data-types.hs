module Adt where

data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True

data FailableDouble
  = Failure
  | OK Double
  deriving (Show)

ex01 = Failure

ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- Store a person's name, age and favourite Thing.

-- Type constructor and Data constructor has the same name
-- Type constructor name Person
-- Data constructor name Person

data Person = Person String Int Thing
  deriving (Show)

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav p@(Person n _ _) = n ++ ", your favorite thing is lame. " ++ show p

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d    -> d

data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

data Tree
  = Leaf Char
  | Node Tree Char Tree
  deriving (Show)

tree :: Tree
tree = Node (Leaf 'x') 'a' (Node (Leaf 'y') 'b' (Leaf 'z'))
