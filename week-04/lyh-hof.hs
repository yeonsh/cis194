-- Higher Order Functions

multThree :: (Num a) => a -> a-> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9
multWithEighteen = multTwoWithNine 9

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred2 = compare 100

divideByTen = (/10)

isUpperAlpha = (`elem` ['A'..'Z'])

applyTwice :: (t -> t) -> t -> t
applyTwice f x = f (f x)

zipWith' :: (a1 -> a2 -> a3) -> [a1] -> [a2] -> [a3]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' f = g
    where g x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted


largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

-- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree1 :: (Num a) => a -> a -> a -> a
addThree1 = \x -> \y -> \z -> x + y + z

flip1' :: (a -> b -> c) -> b -> a -> c
flip1' f = \x y -> f y x

-- only folds and horses

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- sum' [3,5,2,1]
-- 0 + 3
-- 3 + 5
-- 8 + 2
-- 10 + 1
-- acc + first element

sum1' :: (Num a) => [a] -> a
sum1' = foldl (+) 0
-- the lambda function (\acc x -> acc + x) is the same as (+).
-- we can omit the xs as the parameter because calling
-- foldl (+) 0 will return a function that takes a list.
-- Generally, if you have a function like foo a = bar b a
-- you can rewrite it as foo = bar b, because of curring.

elem' :: (Eq a ) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- just to show how it can be implemented using folds.
head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- Function composition is right-associative
-- map (\x -> negate (abs x)) [5,-3,-7,7,2,19,34]
-- map (negate . abs) [5,-3,-7,7,2,19,34]
