{-# OPTIONS_GHC -Wall #-}

-- Validating Credit Card Numbers

-- find the digits of a number
-- e.g. toDigits 1234 == [1,2,3,4]
-- e.g. toDigits 0 = []
-- e.g. toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n
    | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = []

-- reverse the digits of a number
-- e.g. toDigitsRev 1234 == [4,3,2,1]
-- Q. I don't understand why this is needed, as I haven't used it below
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- double every other digit from the right
-- e.g. doubleEveryOther [8,7,6,5] = [16,7,12,5]
-- e.g. doubleEveryOther [1,2,3] = [1,4,3]
-- Q. this doesn't seem the most efficient way of doing it
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ls
    | length ls `mod` 2 == 0 = zipWith (*) (cycle [2,1]) ls
    | otherwise = zipWith (*) (cycle [1,2]) ls

-- sum all the digits in a list
-- e.g. sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- validate credit card number
-- e.g. validate 4012888888881881 = True
-- e.g. validate 4012888888881882 = False
validate :: Integer -> Bool
validate n = check `mod` 10 == 0
    where check = sumDigits $ doubleEveryOther $ toDigits n


-- Tower of Hanoi

type Peg = String
type Move = (Peg, Peg)

-- move n discs from start to finish pegs, with a third temp peg
-- e.g. hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start finish temp
    | n == 0 = []
    | n == 1 = [(start,finish)]
    | otherwise = hanoi (n-1) start temp finish
                  ++ [(start,finish)] 
                  ++ hanoi (n-1) temp finish start