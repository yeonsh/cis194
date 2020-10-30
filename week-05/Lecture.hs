-- More Pholymorphism and type classes

f :: a -> a -> a
-- this does not work
f x y = x && y

-- Haskell does not have anything like Java's instanceOf
-- operator.

-- Haskell types are erased by the compiler after being checked.

-- So, what function actually could have this type?
-- Actually, there are only two

f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y

-- Two views on parametricity
-- not restrictions but guarantees

-- Type classes
-- Num, Eq, Ord and Show are type classes, and
-- we say that (==), (<) and (+) are "type-class polymorphic".
-- type classe correspond to sets of types which have certain operations
-- defined for them, and type class polymorphic functions work only
-- for types which are instances of the type class(es) in question.

data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ = _ = False

  foo1 /= foo2 = not (foo1 == foo2)

-- no need to define /=

data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)

-- tels GHC to automatically derive instances of the Eq, Ord, and Show type
-- classes for our data type Foo.

-- Some standard type classes
-- Ord, Num, Show, Read, Integral

-- A type class example

class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree b)

instance Listable (Tree Int) where
  toList Empty = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

sumL x = sum (toList x)

foo x y = sum (toList x) == sum (toList y) || x < y

instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y
