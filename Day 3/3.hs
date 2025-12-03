import Data.Char (digitToInt)
import Data.Array

-- 1

type Bank = [Int]

readBanks :: String -> [Bank]
readBanks = map (map digitToInt) . lines

computeJoltage :: Int -> Bank -> Int
computeJoltage digits bank = joltages ! (0, digits)
                where joltages = array ((0, 0), (length bank, digits)) [((i, d), opt d i) | i <- [0..length bank], d <- [0..digits]]
                      opt d i | d == 0 || length bank - i < d = 0
                              | otherwise = max pick skip
                        where pick = (bank !! i) * 10 ^ (d - 1) + joltages ! (i + 1, d - 1)
                              skip = joltages ! (i + 1, d)

-- 2

main :: IO()
main = do
        file <- readFile "Day 3/3.txt"
        let banks = readBanks file
            joltages = map (computeJoltage 2) banks
            joltagesLarge = map (computeJoltage 12) banks
        putStrLn $ show $ sum joltages
        putStrLn $ show $ sum joltagesLarge