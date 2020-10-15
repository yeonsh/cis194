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
