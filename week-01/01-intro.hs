sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n -1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3 * n + 1

foo :: Integer -> Integer
foo 0 = 16
foo 1
  | "Haskell" > "C++" = 3
  | otherwise = 4
foo n
  | n < 0 = 0
  | n `mod` 17 == 2 = -43
  | otherwise = n + 3

isEven :: Integer -> Bool
isEven n
  | n `mod` 2 == 0 = True
  | otherwise = False

nums, range, range2 :: [Integer]
nums = [1, 2, 3, 19]
range = [1 .. 100]
range2 = [2, 4 .. 100]

-- Generate the sequence of hailstone iterations from a starting number.
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x : xs) = 1 + intListLength xs

-- Homework

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse $ toDigitsRev x

doubleEveryOtherN :: Integer -> [Integer] -> [Integer]
doubleEveryOtherN _ [] = []
doubleEveryOtherN x (y : ys)
  | x `mod` 2 == 0 = y * 2 : doubleEveryOtherN (x + 1) ys
  | x `mod` 2 == 1 = y : doubleEveryOtherN (x + 1) ys

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ doubleEveryOtherN 1 (reverse x)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = y `mod` 10 == 0 where y = sumDigits $ doubleEveryOther $ toDigits x

-- Hanoi

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a, c)]
  | otherwise = hanoi (n -1) a c b ++ [(a, c)] ++ hanoi (n -1) b a c
