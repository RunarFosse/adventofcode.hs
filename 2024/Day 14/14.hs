import Data.Char
import Data.List
import Data.Set as S hiding (drop, take, foldr, map)

-- 1

type Position = (Int, Int)
type Robot = (Position, Position)

parseRobots :: String -> [Robot]
parseRobots file = foldr (\line r -> parseRobot line : r) [] (lines file)
                 where parseRobot :: String -> Robot
                       parseRobot line = (position, velocity)
                                  where isValidCharacter :: Char -> Bool
                                        isValidCharacter char = isDigit char || char == '-'
                                        (left, right) = ((drop 2 . head . words) line, (drop 2 . head . tail . words) line)
                                        position = ((read . takeWhile isValidCharacter) left, (read . tail . dropWhile isValidCharacter) left)
                                        velocity = ((read . takeWhile isValidCharacter) right, (read . tail . dropWhile isValidCharacter) right)

calculateBounds :: [Robot] -> Position
calculateBounds = foldr (\((px, py), _) (bx, by) -> (1 + max px (bx - 1), 1 + max py (by - 1))) (0, 0)

moveRobots :: [Robot] -> Position -> Int -> [Robot]
moveRobots robots (bx, by) seconds = foldr (\((px, py), (vx, vy)) r -> (((px + vx * seconds) `mod` bx, (py + vy * seconds) `mod` by), (vx, vy)) : r) [] robots

computeSafetyFactor :: [Robot] -> Position -> Int
computeSafetyFactor robots (bx, by) = foldr (*) 1 quadrants
                                    where (cx, cy) = (bx `div` 2, by `div`2)
                                          quadrants = foldr (\robot r -> let quadrant = getQuadrant robot in if quadrant >= 0 then incrementIndex quadrant r else r) [0, 0, 0, 0] robots
                                          incrementIndex :: Int -> [Int] -> [Int]
                                          incrementIndex index as | index == 0 = (1 + as !! 0) : tail as
                                                                  | otherwise = (take index as) ++ [1 + as !! index] ++ (drop (index + 1) as)
                                          getQuadrant :: Robot -> Int
                                          getQuadrant ((px, py), _) | px == cx || py == cy = -1
                                                                    | py < cy = if px < cx then 0 else 1
                                                                    | otherwise = if px < cx then 2 else 3

-- 2

countClusters :: [Robot] -> Int
countClusters robots = foldr (\((px, py), _) r -> r + sum [if S.member (px + a, py + b) positions then 1 else 0 | a <-[-1..1], b <-[-1..1], abs a /= abs b]) 0 robots
                     where positions = (S.fromList . map fst) robots

findMostClusteredSecondAfter :: [Robot] -> Position -> Int -> Int
findMostClusteredSecondAfter robots bounds seconds = snd $ helper robots bounds 0 (0, 0)
                                                where helper :: [Robot] -> Position -> Int -> (Int, Int) -> (Int, Int)
                                                      helper robots bounds iterations (clusters, iteration) | iterations == seconds = (clusters, iteration)
                                                                                                            | otherwise = let currentClusters = countClusters robots in helper (moveRobots robots bounds 1) bounds (iterations + 1) (if currentClusters > clusters then (currentClusters, iterations) else (clusters, iteration))

main :: IO()
main = do
        file <- readFile "Day 14/14.txt"
        let robots = parseRobots file
            bounds = calculateBounds robots
            robotsAfter100 = moveRobots robots bounds 100
            safetyFactorAfter100 = computeSafetyFactor robotsAfter100 bounds
            easterEggSecond = findMostClusteredSecondAfter robots bounds 10000
        putStrLn $ show safetyFactorAfter100
        putStrLn $ show easterEggSecond