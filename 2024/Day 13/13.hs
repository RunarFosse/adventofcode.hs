import Data.Char
import Data.Maybe
import Data.List

-- 1

type Position = (Int, Int)
type Machine = (Position, Position, Position)

extractPosition :: [String] -> Position
extractPosition words = (x, y)
                      where x = (read . filter isDigit . fromJust . find (\word -> head word == 'X')) words
                            y = (read . filter isDigit . fromJust . find (\word -> head word == 'Y')) words

readMachines :: [String] -> [Machine]
readMachines [] = []
readMachines ("":lines) = readMachines lines
readMachines lines = (a, b, prize) : readMachines (drop 3 lines)
                    where a = (extractPosition . words) (lines !! 0)
                          b = (extractPosition . words) (lines !! 1)
                          prize = (extractPosition . words) (lines !! 2)

computePressesToWin :: Machine -> Maybe (Int, Int)
computePressesToWin ((ax, ay), (bx, by), (px, py)) | a `rem` determinant /= 0 || b `rem` determinant /= 0 = Nothing
                                                   | otherwise = Just (a `div` determinant, b `div` determinant)
                                                    where determinant = (ax * by - ay * bx)
                                                          a = (by * px - bx * py) 
                                                          b = (ax * py - ay * px) 
                                                 
computeTokens :: Maybe (Int, Int) -> Int
computeTokens Nothing = 0
computeTokens (Just (a, b)) = 3 * a + b

-- 2

fixMachine :: Machine -> Machine
fixMachine (a, b, (px, py)) = (a, b, (px + 10000000000000, py + 10000000000000))

main :: IO()
main = do
        file <- readFile "Day 13/13.txt"
        let machines = readMachines (lines file)
            presses = map computePressesToWin machines
            tokens = (sum . map computeTokens) presses
            fixedMachines = map fixMachine machines
            fixedPresses = map computePressesToWin fixedMachines
            fixedTokens = (sum . map computeTokens) fixedPresses
        putStrLn $ show tokens
        putStrLn $ show fixedTokens