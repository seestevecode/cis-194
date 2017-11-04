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
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- double every other digit from the right
-- e.g. doubleEveryOther [8,7,6,5] = [16,7,12,5]
-- e.g. doubleEveryOther [1,2,3] = [1,4,3]
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