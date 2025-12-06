import Data.List

-- 1

type Problem = (Char, [String])

readProblems :: String -> [Problem]
readProblems file = readProblemsHelper (lines file) lengths
            where operations = (last . lines) file
                  indices = ((++[length operations + 1]) . map fst . filter ((/= ' ') . snd) . zip [0..]) operations
                  lengths = [next - current - 1 | (current, next) <- zip indices (tail indices)]
                  readProblemsHelper :: [String] -> [Int] -> [Problem]
                  readProblemsHelper _ [] = []
                  readProblemsHelper ls (length:lengths) = readProblem parts : readProblemsHelper remaining lengths
                                    where parts = map (take length) ls
                                          remaining = map (drop (length + 1)) ls

readProblem :: [String] -> Problem
readProblem parts = ((head . last) parts, init parts)

solveProblem :: Problem -> Int
solveProblem (operation, []) = if operation == '+' then 0 else 1
solveProblem ('+', num:nums) = (read num :: Int) + solveProblem ('+', nums)
solveProblem ('*', num:nums) = (read num :: Int) * solveProblem ('*', nums)

-- 2

fixProblem :: Problem -> Problem
fixProblem (operation, nums) = (operation, transpose nums)

main :: IO()
main = do
        file <- readFile "Day 6/6.txt"
        let problems = readProblems file
            answers = (sum . map solveProblem) problems
            problemsFixed = map fixProblem problems
            answersFixed = (sum . map solveProblem) problemsFixed
        putStrLn $ show $ answers
        putStrLn $ show $ answersFixed