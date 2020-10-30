-------------------------------------------------------------------------------
-- Exercise 1: Wholemeal programming

-- fun1 skips odd numbers. with even numbers it subtract two from it
-- and then mutiply the result with following application of the fun1.

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter (even)


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- reimplement using iterate and takeWhile?

fun2' :: Integer -> Integer
fun2' = sum
  . filter even
  . takeWhile (/= 1)
  . iterate (\n -> if even n then n `div` 2 else 3*n+1)

-------------------------------------------------------------------------------
-- Exercise 2: Folding the trees
-- extra Integer to represent the height of a tree at that point

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

-- generate a balanced binary tree from a list of values using foldr.
-- e.g) foldTree "ABCDEFGHIJ"

foldTree :: [a] -> Tree a
foldTree xs = foldr makeTree Leaf xs where

makeTree :: a -> Tree a -> Tree a
makeTree n Leaf = Node 0 Leaf n Leaf
makeTree n (Node d t1 n1 t2)
   | depth t1 < depth t2 = Node (getDepth t1 t2) (makeTree n t1) n1 t2
   | otherwise = Node (getDepth t1 t2) t1 n1 (makeTree n t2)

depth :: Tree a -> Integer
depth Leaf = -1
depth (Node d _ _ _) = d

getDepth :: Tree a -> Tree a -> Integer
getDepth Leaf Leaf = 1
getDepth (Node d1 _ _ _) (Node d2 _ _ _) = 1 + (max d1 d2)
getDepth (Node d _ _ _) Leaf = d + 1
getDepth Leaf (Node d _ _ _) = d + 1

-------------------------------------------------------------------------------
-- Exercise 3: More folds!

-- true if odd number of True

xor :: [Bool] -> Bool
xor xs = foldl countBool False xs
  where countBool = (\x r -> if x then not r else r)

-- implement map as a fold

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> ((f x) : y)) []

-- implement foldl using foldr
-- foldr f z [x1, x2, ..., xn] = x1 `f` (x2 `f` ... (xn `f` z)...)
-- foldl f z [x1, x2, ..., xn] = (...((z `f` x1) `f` x2) `f`...) `f` xn

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (fn f) base xs
  where
    fn f x z = f z x

-------------------------------------------------------------------------------
-- Exercise 4: Finding primes

-- Implement the algorithm Sieve of Sundarama using function composition.
-- Given an integer n, your function should generate all the odd 
-- prime numbers up to 2n + 2.

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = takeWhile (<= 2*n + 2) $ filter odd $ filter isPrime [1..]

-- some help: Cartesian product of two lists.

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- isPrime from https://gist.github.com/marcoscastro/a4c109bb6df608f14bf5
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
  | (length [x|x<-[2..n-1], mod n x == 0]) > 0 = False
  | otherwise = True
