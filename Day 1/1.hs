import Data.Char

-- 1

findFirstDigit :: String -> Int
findFirstDigit [] = 0
findFirstDigit (c:cs) | isDigit c = digitToInt c
                      | otherwise = findFirstDigit cs

extractCalibration :: String -> Int
extractCalibration string = (10 * findFirstDigit string) + (findFirstDigit $ reverse string)

sumCalibrations :: [String] -> Int
sumCalibrations [] = 0
sumCalibrations (string:strings) = extractCalibration string + sumCalibrations strings

-- 2

matchesNumber :: String -> String -> Bool
matchesNumber [] _ = True
matchesNumber _ [] = True
matchesNumber (c1:cs1) (c2:cs2) = c1 == c2 && matchesNumber cs1 cs2

numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

extractNumber :: String -> [String] -> Int
extractNumber _ [] = 0
extractNumber string (number:numbers) | matchesNumber string number = 9 - length numbers
                                      | otherwise = extractNumber string numbers

findFirstRealDigit :: String -> [String] -> Int
findFirstRealDigit [] _ = 0
findFirstRealDigit (c:cs) numbers | isDigit c = digitToInt c
                                  | realNumber > 0 = realNumber
                                  | otherwise = findFirstRealDigit cs numbers
                                  where realNumber = extractNumber (c:cs) numbers

extractRealCalibration :: String -> Int
extractRealCalibration string = (10 * findFirstRealDigit string numbers) + (findFirstRealDigit (reverse string) $ map reverse numbers)

sumRealCalibrations :: [String] -> Int
sumRealCalibrations [] = 0
sumRealCalibrations (string:strings) = extractRealCalibration string + sumRealCalibrations strings

main :: IO()
main = do
    file <- readFile "1.txt"
    putStrLn $ show $ sumCalibrations $ lines file
    putStrLn $ show $ sumRealCalibrations $ lines file