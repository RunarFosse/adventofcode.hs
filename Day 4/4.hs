-- 1

type Card = [String]

extractNumbers :: Card -> ([Int], [Int])
extractNumbers ("|":cs) = ([], map (\n -> read n :: Int) cs)
extractNumbers (c:cs) = ((read c :: Int) : winningNumbers, yourNumbers)
                        where (winningNumbers, yourNumbers) = extractNumbers cs

extractPoints :: [Int] -> [Int] -> Int -> Int
extractPoints _ [] currentPoints = currentPoints
extractPoints winningNumbers (n:ns) currentPoints | any (==n) winningNumbers = extractPoints winningNumbers ns $ if currentPoints == 0 then 1 else 2*currentPoints
                                                  | otherwise = extractPoints winningNumbers ns currentPoints

calculatePoints :: Card -> Int
calculatePoints card = extractPoints winningNumbers yourNumbers 0
                        where (winningNumbers, yourNumbers) = extractNumbers $ drop 2 card

-- 2

extractMatches :: [Int] -> [Int] -> Int
extractMatches _ [] = 0
extractMatches winningNumbers (n:ns) | any (==n) winningNumbers = 1 + extractMatches winningNumbers ns
                                     | otherwise = extractMatches winningNumbers ns

scratchCard :: Card -> Int
scratchCard card = extractMatches winningNumbers yourNumbers
                   where (winningNumbers, yourNumbers) = extractNumbers card

multiply :: [Int] -> Int -> Int -> [Int]
multiply [] _ _ = []
multiply ms multiplier 0 = ms
multiply (m:ms) multiplier n = (multiplier + m) : multiply ms multiplier (n - 1)

countTotalCards :: [Card] -> [Int] -> Int
countTotalCards [] _ = 0
countTotalCards (c:cs) (m:ms) = m + countTotalCards cs (multiply ms m matches)
                                where matches = scratchCard c

main :: IO()
main = do
        file <- readFile "Day 4/4.txt"
        let ls = lines file
            pointsPerCard = map (calculatePoints . words) ls
            totalScratchCards = countTotalCards (map (drop 2 . words) $ ls) (replicate (length ls) 1)
        putStrLn $ (show . sum) pointsPerCard
        putStrLn $ show totalScratchCards