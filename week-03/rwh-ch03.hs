data BookInfo = Book Int String [String]
  deriving (Show)

data MagazineInfo = Magazine Int String [String]
  deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerID String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

-- data Bool = False | True

type CustomerID = Int

type ReviewBody = String

type CardHolder = String

type CardNumber = String

type Address = [String]

data BillingInfo
  = CreditCard CardNumber CardHolder Address
  | CashOnDelivery
  | Invoice CustomerID
  deriving (Show)

a = ("Porpoise", "Grey")

b = ("Table", "Oak")

data Cetacean = Cetacean String String

data Furniture = Furniture String String

c = Cetacean "Porpoise" "Grey"

d = Furniture "Table" "Oak"

-- x and y coordinates or lengths.
data Cartesian2D = Cartesian2D Double Double
  deriving (Eq, Show)

-- Angle and distance (magnitude).
data Polar2D = Polar2D Double Double
  deriving (Eq, Show)

data Roygbiv
  = Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Indigo
  | Violet
  deriving (Eq, Show)

type Vector = (Double, Double)

data Shape
  = Circle Vector Double
  | Poly [Vector]

myNot True  = False
myNot False = True

sumList (x : xs) = x + sumList xs
sumList []       = 0

third (a, b, c) = c

complicated (True, a, x : xs, 5) = (a, xs)

bookID (Book id title authors) = id

bookTitle (Book id title authors) = title

bookAuthors (Book id title authors) = authors

nicerID :: BookInfo -> Int
nicerID (Book id _ _) = id

nicerTitle :: BookInfo -> String
nicerTitle (Book _ title _) = title

nicerAuthors :: BookInfo -> [String]
nicerAuthors (Book _ _ authors) = authors

badExample :: Num a => [a] -> a
badExample (x : xs) = x + badExample xs

goodExample :: Num p => [p] -> p
goodExample (x : xs) = x + goodExample xs
goodExample _        = 0

data Customer = Customer
  { customerID      :: CustomerID,
    customerName    :: String,
    customerAddress :: Address
  }
  deriving (Show)

-- meaning:
-- data Customer = Customer Int String [String]
--                 deriving (Show)

-- customerID :: Customer -> Int
-- customerID (Customer id _ _) = id

-- customerName :: Customer -> String
-- customerName (Customer _ name _) = name

-- customerAddress :: Customer -> [String]
-- customerAddress (Customer _ _ address) = address

customer1 =
  Customer
    271828
    "J.R. Hacker"
    [ "255 Syntax Ct",
      "Milpitas, CA 95134",
      "USA"
    ]

customer2 =
  Customer
    { customerID = 271828,
      customerAddress =
        [ "1048576 Disk Drive",
          "Milpitas, CA 95134",
          "USA"
        ],
      customerName = "Jane Q. Citizen"
    }

-- ghci> :type customerName
-- customerName :: Customer -> String
-- ghci> customerName customer1
-- "J.R. Hacker"

-- data CalendarTime = CalendarTime
--   { ctYear                      :: Int,
--     ctMonth                     :: Month,
--     ctDay, ctHour, ctMin, ctSec :: Int,
--     ctPicosec                   :: Integer,
--     ctWDay                      :: Day,
--     ctYDay                      :: Int,
--     ctTZName                    :: String,
--     ctTZ                        :: Int,
--     ctIsDST                     :: Bool
--   }

-- a is type variable
--  It indicates that the Maybe type takes another type as its parameter.
-- This lets us use Maybe on values of any type.
data Maybe a
  = Just a
  | Nothing

wrapped = Main.Just (Main.Just "wrapped")

-- recursive type
data List a
  = Cons a (List a)
  | Nil
  deriving (Show)

-- recursive type, binary tree, data left right
data Tree a = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

simpleTree :: Tree [Char]
simpleTree = Node "parent" (Node "left child" Empty Empty) (Node "right child" Empty Empty)

mySecond :: [a] -> a
mySecond xs = if null (tail xs)
  then error "list too short"
  else head (tail xs)

safeSecond :: [a] -> Main.Maybe a
safeSecond [] = Main.Nothing
safeSecond xs = if null (tail xs)
  then Main.Nothing
  else Main.Just (head (tail xs))

tidySecond :: [a] -> Main.Maybe a
tidySecond (_:x:_) = Main.Just x
tidySecond _       = Main.Nothing

lend :: (Ord a, Num a) => a -> a -> Main.Maybe a
lend amount balance = let reserve = 100
                          newBalance = balance - amount
  in if balance < reserve
    then Main.Nothing
    else Main.Just newBalance

-- nested let blocks
-- not recommended
foo = let a = 1
      in let b = 2
         in a + b

-- nested let blocks
-- not recommended to use same name
bar = let x = 1
      in ((let x = "foo" in x), x)

quux a = let a = "foo"
         in a ++ "eek!"

lend2 amount balance = if amount < reserve * 0.5
                       then Main.Just newBalance
                       else Main.Nothing
  where reserve = 100
        newBalance = balance - amount

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
  where plural 0 = "no " ++ word ++ "s"
        plural 1 = "one " ++ word
        plural n = show n ++ " " ++ word ++ "s"

bar1 = let a = 1
           b = 2
           c = 3
       in a + b + c

foo1 = let { a = 1;  b = 2;
        c = 3 }
      in a + b + c

fromMaybe defval wrapped =
    case wrapped of
      Main.Nothing    -> defval
      Main.Just value -> value

-- INCORRECT
data Fruit = Apple | Peach

apple = "apple"

peach = "peach"

whichFruit :: String -> Fruit

whichFruit f = case f of
                 apple -> Apple
                 peach -> Peach -- apple, peach is matching anything. local pattern variable.

betterFruit f = case f of
                  "apple" -> Apple
                  "peach" -> Peach

-- bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
-- bad_nodesAreSame _            _            = Nothing

nodesAreSame (Node a _ _) (Node b _ _)
    | a == b     = Main.Just a
nodesAreSame _ _ = Main.Nothing

lend3 amount balance
     | amount <= 0            = Main.Nothing
     | amount > reserve * 0.5 = Main.Nothing
     | otherwise              = Main.Just newBalance
    where reserve    = 100
          newBalance = balance - amount

-- We can use guards anywhere that we can use patterns.

myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

niceDrop n xs     | n <= 0 = xs
niceDrop _ []     = []
niceDrop n (_:xs) = niceDrop (n - 1) xs
