greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter gt100 xs where
    gt100 x = x > 100

greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 xs = filter (\x -> x > 100) xs

greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 xs = filter (>100) xs

-- (?y) is equivalent to the function \x -> x ? y
-- (y?) is equivalent to the function \x -> y ? x

-- Function composition

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)

myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

-- myTest' is just a "pipeline" compsed of three
-- smaller functions. Function composition seems 'backwards'.
-- Its' because function application is backwards.
-- We stuck with the backward notation since 1734 no thanks to
-- Alexis Claude Clairaut and Euler.

-- Currying and partial application

f :: Int -> Int -> Int
f x y = 2*x + y

-- All functions in Haskell take only one argument.

f' :: Int -> (Int -> Int)
f' x y = 2* x + y

-- Function arrows associate to the right.
-- w -> x -> y -> z == w -> (x -> (y -> z))

-- Function application, in turn, is left-associative.
-- f 3 2 == (f 3) 2
-- It means only one argument applies to function and it returns a function.

-- \x y z -> ...
-- \x -> (\y -> (\z -> ...))

-- f x y z = ...
-- f = \x -> (\y -> (\z -> ...))

-- currying: idea of representing multi-argument functions as
-- one-argument functions returing functions.
-- Named after British mathematician and logician Haskell Curry(1900-1982).

f'' :: (Int, Int) -> Int
f'' (x, y) = 2 * x + y

schönfinkel :: ((a,b) -> c) -> a -> b -> c
schönfinkel f x y = f (x,y)

unschönfinkel :: (a -> b -> c) -> (a,b) -> c
unschönfinkel f (x,y) = f x y

-- uncurry (+) (2, 3)
-- 5

-- There are no functions of multiple arguments in Haskell.

foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x:xs)
    | x > 3 = (7*x + 2) + foobar xs
    | otherwise = foobar xs
-- not good Haskell style. Doing too much at once and working at too low of a level.

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)
-- pipelines of three functions

-- Folds

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' [] = 0
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

fold :: b -> (a -> b -> b) -> [a] -> b
fold z f [] = z
fold z f (x:xs) = f x (fold z f xs)

-- fold f z [a,b,c] == a `f` (b `f` ( c `f` z))

sum'' = fold 0 (+)
product'' = fold 1 (*)
length'' = fold 0 (\_ s -> 1 + s)
-- length'' = fold 0 (\_ -> (1+))
-- length'' = fold 0 (const (1+))

-- some Prelude functions which are defined in terms of foldr
-- length :: [a] -> Int
-- sum :: Num a => [a] -> a
-- product :: Num a => [a] -> a
-- and :: [Bool] -> Bool
-- or :: [Bool] -> Bool
-- any :: (a -> Bool) -> [a] -> Bool
-- all :: (a -> Bool) -> [a] -> Bool

-- foldr fz [a,b,c] = a `f` (b `f` (c `f` z))
-- foldl fz [a,b,c] = ((z `f` a) `f` b) `f` c