import Data.List
import Data.Char
import Data.Bits

-- 1

type State = (Int, Int, Int)
type Program = [Int]

parseInput :: String -> (State, Program)
parseInput file = (state, program)
                where ls = lines file
                      state = (\[a, b, c] -> (a, b, c)) $ map (read . last . words) (take 3 ls)
                      program = (map (read . (:"")) . filter isDigit . last) ls

combo :: State -> Int -> Int
combo (a, _, _) 4 = a
combo (_, b, _) 5 = b
combo (_, _, c) 6 = c
combo _ 7 = error "Invalid combo operand"
combo _ operand = operand

compute :: Program -> Int -> State -> [Int]
compute program pointer state@(a, b, c) | pointer > length program - 2 = []
                                        | otherwise = case (take 2 . drop pointer) program of
                                        [0, operand] -> compute program (pointer + 2) (a `div` (2 ^ (combo state operand)), b, c)
                                        [1, operand] -> compute program (pointer + 2) (a, xor b operand, c)
                                        [2, operand] -> compute program (pointer + 2) (a, combo state operand `mod` 8, c)
                                        [3, operand] -> if a == 0 then compute program (pointer + 2) state else compute program operand state
                                        [4, operand] -> compute program (pointer + 2) (a, xor b c, c)
                                        [5, operand] -> combo state operand `mod` 8 : compute program (pointer + 2) state
                                        [6, operand] -> compute program (pointer + 2) (a, a `div` (2 ^ (combo state operand)), c)
                                        [7, operand] -> compute program (pointer + 2) (a, b, a `div` (2 ^ (combo state operand)))

showOutput :: [Int] -> String
showOutput = intercalate "," . map show

-- 2

computePartialQuine :: Program -> State -> [Int] -> Int -> [Int]
computePartialQuine program state@(_, b, c) ns operands = filter (\n -> compute program 0 (n, b, c) == partialProgram) (concat queues)
                                                        where partialProgram = drop (length program - operands) program
                                                              queues = map (\n -> [shift n 3..(shift n 3) + 7]) ns
                                                              
computeQuine :: Program -> State -> Int
computeQuine program state = head $ foldl (computePartialQuine program state) [0] [1..length program]

main :: IO()
main = do
        file <- readFile "Day 17/17.txt"
        let (state, program) = parseInput file
            output = compute program 0 state
            quine = computeQuine program state
        putStrLn $ showOutput output
        putStrLn $ show quine