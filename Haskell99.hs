-- 99 Haskell Problems :)
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems


-- Problem 1
-- (*) Find the last element of a list.
last' :: [a] -> Maybe a 
last' []  = Nothing
last' [x] = Just x 
last' (_:xs) = last' xs 

-- Problem 2
-- (*) Find the last but one element of a list.
lastBut :: [a] -> Maybe a 
lastBut xs = case xs of
  []      -> Nothing
  [x,_]   -> Just x 
  (_:xs') -> lastBut xs'

-- Problem 3
-- (*) Find the K'th element of a list. 
-- The first element in the list is number 1.
type ErrMes = String
elAt :: Int -> [a] -> Either ErrMes a
elAt i xs 
  | i > length xs = Left "Index too big"
  | otherwise = Right $ elAt' i 1 xs where 
    elAt' ix iy (x:xs') 
      | ix == iy = x 
      | otherwise = elAt' ix (iy + 1) xs'

-- Problem 4
-- (*) Find the number of elements of a list.
listLen :: [a] -> Integer
listLen  = foldr (\_ ac -> ac + 1)  0

-- Problem 5
-- (*) Reverse a list.
rev :: [a] -> [a]
rev = foldr (\x ac -> ac ++ [x]) []

-- Problem 6
-- (*) Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward;
-- e.g. (x a m a x).
isPalin :: Eq a => [a] -> Bool
isPalin xs 
  | xs == rev xs = True
  | otherwise = False

-- Problem 7
-- (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements 
-- into a `flat' list by replacing each list with its elements.
flatten :: [[a]] -> [a]
flatten [] = []
flatten xs = foldr (++) [] xs

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements 
-- they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
removeDups :: Eq a =>  [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups ( dropWhile (==x)  xs)


-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. 
-- If a list contains  repeated elements they should be 
-- placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs ) : pack ( dropWhile (==x)  xs)
 
-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem 
-- P09 to implement the so-called run-length encoding data compression
--  method. Consecutive duplicates of elements are encoded as 
-- lists (N E) where N is the number of duplicates of the element E.
-- Example:
-- * (encode '(a a a a b c c a a d e e e e))
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
encode :: Eq a => [a] -> [(Int, a)]
encode = code . pack where 
    code = foldr (\vals@(x:_) ac -> (length vals, x):ac) []

-- Problem 11
-- (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an 
-- element has no duplicates it is simply copied into the result list. 
-- Only elements with duplicates are transferred as (N E) lists.
-- Example in Haskell:

-- P11> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']

data Code a = Single a | Multi Int a deriving Show
encode' :: Eq a => [a] -> [Code a]
encode' xs = foldr f [] (encode xs) 
    where 
        f (x, y) ac
         | x == 1 = Single y : ac
         | otherwise = Multi x y : ac

-- Problem 12 A
decode :: Eq a => [(Int, a)] -> [a] 
decode [] = []
decode xs = foldr f [] xs where
    f (0, _) lst = lst
    f (q, v) lst = v : f (q-1, v) lst


-- Problem 14
-- (*) Duplicate the elements of a list.
-- Problem 22
-- Create a list containing all integers within a given range.
dupl :: [a] -> [a]
dupl = foldr (\x ac -> x:x:ac) []

-- Problem 15
-- (**) Replicate the elements of a list a given number of times.
replN :: Integer -> [a] -> [a]
replN n = foldr (f n) []  where
  f 0 _ xs' =  xs'
  f i x' xs' = f (i-1) x' (x':xs') 

-- Problem 16
-- (**) Drop every N'th element from a list.
dropN :: Integer -> [a] -> [a]
dropN n xs = foldr (\x ac -> f  x ac ) [] xs where



range :: Integer -> Integer -> [Integer] 
range f l 
 | f > l = []
 | otherwise = f : range (f + 1) l

range' :: Integer -> Integer -> [Integer] 
range' f l
  | f > l = []
  | otherwise = foldl fr [f]  $  range' (f+1) l 
    where 
     fr xs x =  xs ++ [x]

range'' :: Integer -> Integer -> [Integer] 
range'' f l
  | f > l = []
  | otherwise = foldr (:) [f] $ range'' (f+1) l





