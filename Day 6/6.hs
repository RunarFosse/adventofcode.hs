
-- 1

type Race = (Int, Int)

createRaces :: [String] -> [String] -> [Race]
createRaces [] [] = []
createRaces (time:ts) (dist:ds) = (read time, read dist) : createRaces ts ds

calculateWaysToWin :: Int -> Int -> Int -> Int
calculateWaysToWin _ 0 _ = 0
calculateWaysToWin record time speed | speed * time > record = time - (speed - 1)
                                     | otherwise = calculateWaysToWin record (time - 1) (speed + 1)

iterateRaces :: [Race] -> [Int]
iterateRaces [] = []
iterateRaces ((time, dist):races) = calculateWaysToWin dist time 0 : iterateRaces races

-- 2

createCorrectRace :: [String] -> [String] -> (String, String) -> Race
createCorrectRace [] [] (time, dist) = (read time, read dist)
createCorrectRace (t:ts) (d:ds) (time, dist) = createCorrectRace ts ds (time ++ t, dist ++ d)

main :: IO()
main = do
        file <- readFile "Day 6/6.txt"
        let ls = lines file
            races = createRaces (tail $ words $ ls !! 0) (tail $ words $ ls !! 1)
            waysToWin = iterateRaces races
            (correctTime, correctDist) = createCorrectRace (tail $ words $ ls !! 0) (tail $ words $ ls !! 1) ("", "")
        putStrLn $ show $ product waysToWin
        putStrLn $ show $ calculateWaysToWin correctDist correctTime 0
