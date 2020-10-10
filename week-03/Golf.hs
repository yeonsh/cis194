module Golf where

import Data.List

-- Exercise 1 Hopscotch

-- the first list should be the same as the input list.
-- the second list contain every second element from the input list.
-- the nth list should contain every nth element from the input list.

-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips [1] == [[1]]
-- skips [True, False] = [[True, False], [False]]


skips :: [a] -> [[a]]
skips xs = [n | i <- [0..(length xs)-1], let n = makelist i (length xs) xs]

makelist :: Int -> Int -> [bs] -> [bs]
makelist n m bs = [bs!!j|i<-[n..m], let j = n + (i-n) * (n+1), j < m]

-- Exercise 2 Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | x < y && y > z = y : localMaxima(y:z:zs)
  | otherwise = localMaxima(y:z:zs)
localMaxima _ = []

-- Exercise 3 Histogram
-- histogram [1,1,1,5] ==
--  *
--  *
--  *   *
-- ==========
-- 0123456789

histogram :: [Integer] -> String
histogram xs =
  do {
    let n = [(toInteger k)|i <- [0..9], let k = length $ filter (==(toInteger i)) xs]
    ; let m = toInteger $ maximum n
    ; concat $ map (getHistogramLine n) [m, m-1..1]
  } ++ "==========\n0123456789\n"

getHistogramLine :: [Integer] -> Integer -> String
getHistogramLine ns m = [c|i<-ns, let c = starOrBlank i m] ++ "\n"

starOrBlank :: Integer -> Integer -> Char
starOrBlank i m1
  | i >= m1 = '*'
  | otherwise = ' '
