module Lecture where

-- Recursion patterns

data IntList = Empty | Cons Int IntList
    deriving Show

-- Perform some operation on every element of the list
-- Keep only some elements of the list, and throw others away, based on a test
-- "Summarize" the elements of the list somehow (find their sum, product, maximum...)
-- You can probably think of others

-- Map

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x*x) (squareAll xs)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty       = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

addOne :: Int -> Int
addOne x = x + 1

square :: Int -> Int
square x = x * x

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

-- Filter

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
    | even x = Cons x (keepOnlyEven xs)
    | otherwise = keepOnlyEven xs

-- Polymorphic data types

data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

-- Polymorphic functions

filterList _ E = E
filterList p (C x xs)
    | p x = C x (filterList p xs)
    | otherwise = filterList p xs
-- what is the type of filterList?

mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)

-- Replacing partial functions

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + head (tail xs)

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2

-- Writing partial functions

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
