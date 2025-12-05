import Data.List

-- 1

type Range = (Int, Int)

parseRange :: String -> Range
parseRange string = (read left :: Int, read right :: Int)
                where (left, right) = (takeWhile (/= '-') string, tail $ dropWhile (/= '-') string)

parseDatabase :: String -> ([Range], [Int])
parseDatabase file = (ranges, ids)
                where (upper, lower) = ((takeWhile (not . null) . lines) file, (tail . dropWhile (not . null) . lines) file)
                      ranges = (sort . map parseRange) upper
                      ids = (sort . map (\id -> read id :: Int)) lower

countFreshIds :: [Range] -> [Int] -> Int
countFreshIds [] _ = 0
countFreshIds _ [] = 0
countFreshIds ((start, end):ranges) (id:ids) | id > end = countFreshIds ranges (id:ids)
                                             | otherwise = (if id < start then 0 else 1) + countFreshIds ((start, end):ranges) ids

-- 2

countFresh :: [Range] -> Int
countFresh ranges = countFreshHelper ranges 0
                where countFreshHelper :: [Range] -> Int -> Int
                      countFreshHelper [] _ = 0
                      countFreshHelper ((start, end):ranges) lastEnd | lastEnd < start = (end - start + 1) + countFreshHelper ranges end
                                                                     | lastEnd > end = countFreshHelper ranges lastEnd
                                                                     | otherwise = (end - lastEnd) + countFreshHelper ranges end

main :: IO()
main = do
        file <- readFile "Day 5/5.txt"
        let (ranges, ids) = parseDatabase file
            freshIds = countFreshIds ranges ids
            fresh = countFresh ranges
        putStrLn $ show $ freshIds
        putStrLn $ show $ fresh