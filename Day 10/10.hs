import Data.Set as S hiding (map, filter, null)
import Data.Char (isDigit)
import System.Process

-- 1

type Machine = ([Bool], [Set Int], [Int])

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c' (c:cs) | c == c' = "" : splitOn c' cs
                  | otherwise = (c:current) : rest
                    where (current:rest) = splitOn c' cs

readMachine :: String -> Machine
readMachine line = (target, buttons, joltages)
                where parts = words line
                      target = [char == '#' | char <- (init . tail . head) parts]
                      buttons = (map (S.fromList . map read . splitOn ',' . init . tail) . init . tail) parts
                      joltages = (map read . splitOn ',' . init . tail . last) parts

initialState :: Machine -> ([Bool], Int)
initialState (target, _, _) = (replicate (length target) False, 0)

toggleMachine :: Machine -> [([Bool], Int)] -> Set [Bool] -> Int
toggleMachine machine@(target, buttons, _) ((state, steps):queue) seen | state == target = steps
                                                                       | S.member state seen = toggleMachine machine queue seen
                                                                       | otherwise = toggleMachine machine (queue ++ neighbours) (S.insert state seen)
                                        where neighbours = [([if S.member i toggles then (not light) else light | (i, light) <- zip [0..] state], steps + 1) | toggles <- buttons]

-- 2

generateSMT :: Machine -> String
generateSMT (_, buttons, joltages) = unlines [variables, positiveConstraints, joltageConstraints, objective, "(check-sat)", "(get-objectives)"]
    where variables = unlines ["(declare-const x" ++ show i ++ " Int)" | i <- [0..length buttons - 1]]
          positiveConstraints = unlines ["(assert (>= x" ++ show i ++ " 0))" | i <- [0..length buttons - 1]]
          joltageSuppliers :: Int -> String 
          joltageSuppliers i = "(+ " ++ unwords ["x" ++ show j | (j, button) <- zip [0..] buttons, S.member i button] ++ ")"
          joltageConstraints = unlines ["(assert (= " ++ joltageSuppliers i ++ " " ++ show (joltages !! i) ++ "))" | i <- [0..length joltages - 1]]        
          objective = "(minimize (+ " ++ unwords ["x" ++ show i | i <- [0..length buttons - 1]] ++ "))"        

powerMachine :: Machine -> IO Int
powerMachine machine = readCreateProcess (proc "z3" ["-in"]) input >>= (return . parseOutput)
                    where input = generateSMT machine
                          parseOutput output = (read . init . last . words) (lines output !! 2)

main :: IO()
main = do
        file <- readFile "Day 10/10.txt"
        let machines = map readMachine (lines file)
            steps = map (\machine -> toggleMachine machine [initialState machine] S.empty) machines
        joltageSteps <- mapM powerMachine machines
        putStrLn $ show $ sum steps
        putStrLn $ show $ sum joltageSteps