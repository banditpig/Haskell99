-- 99 Haskell Problems :)
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems


-- Problem 1
-- (*) Find the last element of a list.
import Data.List
import Data.Char

last' :: [a] -> Maybe a 
last' []  = Nothing
last' [x] = Just x 
last' (_:xs) = last' xs 
-- ---------------------------------------------------

-- Problem 2
-- (*) Find the last but one element of a list.
lastBut :: [a] -> Maybe a 
lastBut xs = case xs of
  []      -> Nothing
  [x,_]   -> Just x 
  (_:xs') -> lastBut xs'
-- ---------------------------------------------------

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
-- ---------------------------------------------------

-- Problem 4
-- (*) Find the number of elements of a list.
listLen :: [a] -> Integer
listLen  = foldr (\_ ac -> ac + 1)  0
-- ---------------------------------------------------

-- Problem 5
-- (*) Reverse a list.
rev :: [a] -> [a]
rev = foldr (\x ac -> ac ++ [x]) []
-- ---------------------------------------------------
-- Problem 6
-- (*) Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward;
-- e.g. (x a m a x).
isPalin :: Eq a => [a] -> Bool
isPalin xs 
  | xs == rev xs = True
  | otherwise = False
-- ---------------------------------------------------

-- Problem 7
-- (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements 
-- into a `flat' list by replacing each list with its elements.
flatten :: [[a]] -> [a]
flatten [] = []
flatten xs = foldr (++) [] xs
-- ---------------------------------------------------

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements 
-- they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
removeDups :: Eq a =>  [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups ( dropWhile (==x)  xs)
-- ---------------------------------------------------


-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. 
-- If a list contains  repeated elements they should be 
-- placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs ) : pack ( dropWhile (==x)  xs)
-- ---------------------------------------------------
 
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
-- ---------------------------------------------------

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
-- ---------------------------------------------------

-- Problem 12 A
decode :: Eq a => [(Int, a)] -> [a] 
decode [] = []
decode xs = foldr f [] xs where
    f (0, _) lst = lst
    f (q, v) lst = v : f (q-1, v) lst
-- ---------------------------------------------------
-- Problem 14
-- (*) Duplicate the elements of a list.

dupl :: [a] -> [a]
dupl = foldr (\x ac -> x:x:ac) []
-- ---------------------------------------------------

-- Problem 15
-- (**) Replicate the elements of a list a given number of times.
replN :: Integer -> [a] -> [a]
replN n = foldr (f n) []  where
  f 0 _ xs' =  xs'
  f i x' xs' = f (i-1) x' (x':xs') 
-- ---------------------------------------------------

-- Problem 16
-- (**) Drop every N'th element from a list.
dropN :: Eq a => Int -> [a] -> [a]
dropN _ [] = []
dropN 0 xs = xs
dropN 1 _  = []
dropN n xs 
 | n > length xs = xs 
 | otherwise = filter f xs where 
  f el = case elemIndex el xs of
    Nothing -> False
    Just ix -> rem ix n == 0
-- ---------------------------------------------------

-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
splitTwo :: Eq a =>  Int -> [a] -> ([a], [a])
splitTwo n xs = (takeN n xs, removeN n xs) 

splitTwo' :: Eq a =>  Int -> [a] -> ([a], [a]) 
splitTwo' _ [] = ([], [])
splitTwo' n lst@(x:xs)
  | n > 0 = (x : ys, zs)
  | otherwise = ([], lst) where
     (ys, zs) = splitTwo'  (n - 1) xs

-- split l@(x : xs) n | n > 0     = (x : ys, zs)
--                    | otherwise = ([], l)
--     where (ys,zs) = split xs (n - 1)

takeN :: Eq a => Int -> [a] -> [a]
takeN 0 _ = []
takeN _ [] = []
takeN n xs 
  | n >= length xs = xs
  | otherwise = takeN' n n xs where
    takeN' ix n' (x':xs')
      | ix == 0 = []
      | otherwise = x' : takeN' (ix - 1) n' xs'

removeN :: Eq a => Int -> [a] -> [a]
removeN 0 xs = xs
removeN _ [] = []
removeN n xs
  | n >= length xs = []
  | otherwise = removeN' n n xs where 
    removeN' ix n' lst@xs'
      | ix == 0 = lst
      | otherwise = removeN' (ix - 1) n' xs'
-- ---------------------------------------------------
-- Problem 18
-- (**) Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the 
-- elements between the i'th and j'th element of 
-- the original list (both limits included).
-- Example:
-- slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

slice :: [a] -> Int -> Int -> [a]
slice xs  = buildSlice xs 0  where 
  buildSlice [] _ _ _ = []
  buildSlice (x':xs') ix i j 
    | ix >= i && ix <= j = x' : buildSlice xs' (ix + 1) i j
    | otherwise = buildSlice xs' (ix + 1) i j

-- ---------------------------------------------------
-- Problem 19
-- (**) Rotate a list N places to the left.

-- rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate xs i 
  | i < 0 = rotate xs (length xs  - abs i) 
  | otherwise = drop i xs ++ slice xs 0 (i - 1)
-- ---------------------------------------------------
-- Problem 20
-- (*) Remove the K'th element from a list.
-- removeAt 2 "abcd"
-- ('c',"abd")
removeAt :: Int -> [a] -> (a, [a])
removeAt i xs = (x, xs') where
  [x] = slice xs i i
  xs' = slice xs 0 (i - 1) ++ slice xs (i + 1) (length xs)
-- ---------------------------------------------------
-- Problem 21
-- Insert an element at a given position into a list.

-- insertAt 'X' "abcd" 2
-- "abXcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = slice xs 0 (i - 1) ++ x : slice xs i (length xs)

-- ---------------------------------------------------
-- Problem 22
-- Create a list containing all integers within a given range.
-- range 4 9
-- [4,5,6,7,8,9]

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
-- ---------------------------------------------------
--
-- Problems 21 - 25 later 
--
-- ---------------------------------------------------

-- Problem 26
-- (**) Generate the combinations of K distinct objects chosen 
-- from the N elements of a list
-- In how many ways can a committee of 3 be chosen from a group of 12 people? 
-- We all know that there are C(12,3) = 220 
-- possibilities (C(N,K) denotes the well-known binomial coefficients).
-- For pure mathematicians, this result may be great.
-- But we want to really generate all the possibilities in a list.
-- combs 3 "abcdef"
-- ["abc","abd","abe",...]
combs :: Integer -> [a] -> [[a]]
combs i (x:xs) =  foldr (\_ ac -> f x ac) []  [0..nck] 
  where 
    nck = nCk len i
    len = toInteger (length xs)
    f x ac = [x]:ac

--listN :: Integer -> [a] -> [a]
listN n xs  = [] 
fac :: Integer -> Integer
fac n = product [1..n]
-- nCk = n! /  (k! (n-k)! )
nCk n k  = fac n `div` (fac (n-k) * fac k)


-- Not part of 99 Problems.
-- Get the digits as a list of lists discarding any non-digits
-- extractNumbers "a12a4" = ["12","4"]
extractNumbers :: String -> [String]
extractNumbers [] = []
extractNumbers xs = nums : extractNumbers xs' where
  (nums, xs') = span isDigit $ dropWhile ( not . isDigit ) xs

