-- 1

type Report = [Int]

parseReports :: [String] -> [Report]
parseReports = map (map (\x -> read x :: Int) . words)

isSafe :: Report -> Bool
isSafe report = isDecreasing report || isIncreasing report

isDecreasing :: Report -> Bool
isDecreasing (n1:n2:ns) = n1 > n2 && abs (n2 - n1) < 4 && isDecreasing (n2:ns)
isDecreasing _ = True

isIncreasing :: Report -> Bool
isIncreasing (n1:n2:ns) = n1 < n2 && abs (n2 - n1) < 4 && isIncreasing (n2:ns)
isIncreasing _ = True
            
-- 2

isDampenedSafe :: Report -> Bool
isDampenedSafe report = isDecreasing (tail report) || isIncreasing (tail report) || isDampenedDecreasing report || isDampenedIncreasing report

isDampenedDecreasing :: Report -> Bool
isDampenedDecreasing (n1:n2:ns) | n1 > n2 && abs (n2 - n1) < 4 = isDampenedDecreasing (n2:ns)
                                | otherwise = isDecreasing (n1:ns)
isDampenedDecreasing _ = True

isDampenedIncreasing :: Report -> Bool
isDampenedIncreasing (n1:n2:ns) | n1 < n2 && abs (n2 - n1) < 4 = isDampenedIncreasing (n2:ns)
                                | otherwise = isIncreasing (n1:ns)
isDampenedIncreasing _ = True

main :: IO()
main = do
        file <- readFile "Day 2/2.txt"
        let reports = parseReports $ lines file
            safeReports = (length . filter id . map isSafe) reports
            dampenedSafeReports = (length . filter id . map isDampenedSafe) reports
        putStrLn $ show safeReports
        putStrLn $ show dampenedSafeReports
