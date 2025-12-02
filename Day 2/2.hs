import Data.List

-- 1

type Range = (Int, Int)

extractRange :: String -> Range
extractRange string = (read left :: Int, read right :: Int)
                    where left = takeWhile (/= '-') string
                          right = (tail . dropWhile (/= '-')) string

extractRanges :: String -> [Range]
extractRanges "" = []
extractRanges file = range : extractRanges rest
                    where range = (extractRange . takeWhile (/= ',')) file
                          rest = (tail . dropWhile (/= ',')) file

countDigits :: Int -> Int
countDigits 0 = 0
countDigits num = 1 + countDigits (div num 10)

computeInvalids :: Int -> Range -> [Int]
computeInvalids repeats (left, right) = (takeWhile (<= right) . dropWhile (< left)) candidates
                            where digits = countDigits left
                                  leftprefix = div left $ 10 ^ (digits - div digits repeats)
                                  candidates = map (\suffix -> sum $ map (\i -> suffix * 10 ^ (countDigits suffix * i)) [0..repeats-1]) [leftprefix..]

-- 2

primes :: [Int]
primes = sieve [2..]
        where sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]

computeDeepInvalids :: Range -> [Int]
computeDeepInvalids (left, right) = nub invalids
                              where factors = takeWhile (<= countDigits right) primes
                                    invalids = foldl (\invalids repeats -> computeInvalids repeats (left, right) ++ invalids) [] factors

main :: IO()
main = do
        file <- readFile "Day 2/2.txt"
        let ranges = extractRanges (file ++ ",")
            invalids = map (sum . computeInvalids 2) ranges
            invalidsDeep = map (sum . computeDeepInvalids) ranges
        putStrLn $ show $ sum invalids
        putStrLn $ show $ sum invalidsDeep
        