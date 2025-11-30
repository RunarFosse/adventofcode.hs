import Data.Map as M hiding (map, filter, null)
import Data.Set as S hiding (map, filter, null)

-- 1

type Rules = Map Int [Int]
type Update = [Int]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn elem as = case break (==elem) as of
                (a, elem:b) -> a : splitOn elem b
                (a, [])    -> [a]

parseInput :: String -> (Rules, [Update])
parseInput file = (parseRules left, map (map (\s -> read s :: Int) . splitOn ',') $ tail right)
                where (left, right) = (span (/="") . lines) file

parseRules :: [String] -> Rules
parseRules [] = M.empty
parseRules (line:lines) = M.insert second (first : M.findWithDefault [] second rules) rules
                        where [first, second] = (map (\s -> read s :: Int) . splitOn '|') line
                              rules = parseRules lines

isCorrectOrder :: Rules -> Set Int -> Update -> Bool
isCorrectOrder _ _ [] = True
isCorrectOrder rules elems (n:ns) = all (\e -> S.notMember e elems) required && isCorrectOrder rules (S.delete n elems) ns
                                  where required = M.findWithDefault [] n rules
                                      

findMiddle :: [a] -> a
findMiddle as = as !! middle
              where middle = length as `div` 2

-- 2

fixOrder :: Rules -> Set Int -> Set Int -> Update -> Update
fixOrder _ _ _ [] = []
fixOrder rules elems seen (n:ns) | S.member n seen = fixOrder rules elems seen ns
                           | null missing = n : fixOrder rules (S.delete n elems) (S.insert n seen) ns
                           | otherwise = fixOrder rules elems seen (missing ++ (n:ns))
                           where required = M.findWithDefault [] n rules
                                 missing = filter (\e -> S.member e elems && S.notMember e seen) required

main :: IO()
main = do
        file <- readFile "Day 5/5.txt"
        let (rules, updates) = parseInput file
            correctUpdates = filter ((\update -> isCorrectOrder rules (S.fromList update) update)) updates 
            correctMiddles = map findMiddle correctUpdates
            incorrectUpdates = filter (not . (\update -> isCorrectOrder rules (S.fromList update) update)) updates 
            fixedMiddles = map (findMiddle . (\update -> fixOrder rules (S.fromList update) S.empty update)) incorrectUpdates
        putStrLn $ show $ sum correctMiddles
        putStrLn $ show $ sum fixedMiddles
