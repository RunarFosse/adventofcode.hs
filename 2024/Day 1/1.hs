import Data.List

-- 1

parseInput :: String -> ([Int], [Int])
parseInput file = (sort $ map (\l -> read (l !! 0) :: Int) lists, sort $ map (\l -> read (l !! 1) :: Int) lists)
                where lists = map words $ lines file

sumDifferences :: [Int] -> [Int] -> Int
sumDifferences [] [] = 0
sumDifferences (n1:n1s) (n2:n2s) = abs (n1 - n2) + sumDifferences n1s n2s

-- 2

computeSimilarity :: [Int] -> [Int] -> Int
computeSimilarity _ [] = 0
computeSimilarity [] _ = 0
computeSimilarity (n1:n1s) (n2:n2s) | n1 == n2 = n1 + computeSimilarity (n1:n1s) n2s
                                    | n1 < n2 = computeSimilarity n1s (n2:n2s)
                                    | n1 > n2 = computeSimilarity (n1:n1s) n2s
                                    | otherwise = computeSimilarity n1s n2s

main :: IO()
main = do
        file <- readFile "Day 1/1.txt"
        let (list1, list2) = parseInput file
        putStrLn $ show $ sumDifferences list1 list2
        putStrLn $ show $ computeSimilarity list1 list2
        