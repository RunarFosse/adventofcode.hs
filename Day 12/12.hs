import Data.List
import qualified Data.Map as M

-- 1

type Record = (String, [Int])

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c' (c:cs) | c == c' = "" : splitOn c' cs
                  | otherwise = (c:current) : rest
                    where (current:rest) = splitOn c' cs

createRecord :: String -> Record
createRecord line = (arrangement, map (\n -> read n :: Int) $ splitOn ',' groups)
                    where [arrangement, groups] = words line

countPossibleArrangements :: Record -> Int
countPossibleArrangements ("", groups) = if groups == [] then 1 else 0
countPossibleArrangements (arrangement, []) = if any (=='#') arrangement then 0 else 1
countPossibleArrangements ('.':arrangement, groups) = countPossibleArrangements (arrangement, groups)
countPossibleArrangements ('#':arrangement, (n:ns)) | length ('#':arrangement) < sum (n:ns) = 0
                                                    | (length arrangement >= n && arrangement !! (n-1) == '#') || any (=='.') (take (n-1) arrangement) = 0
                                                    | otherwise = countPossibleArrangements (drop n arrangement, ns)
countPossibleArrangements ('?':arrangement, (n:ns)) | length ('?':arrangement) < sum (n:ns) = 0
                                                    | (length arrangement >= n && arrangement !! (n-1) == '#') || any (=='.') (take (n-1) arrangement) = countPossibleArrangements (arrangement, (n:ns))
                                                    | otherwise = countPossibleArrangements (arrangement, (n:ns)) + countPossibleArrangements (drop n arrangement, ns)

-- 2 (Dynamic programming >_<)

unfoldRecord :: Record -> Record
unfoldRecord (arrangement, groups) = ((intercalate "?" . replicate 5) arrangement, (concat . replicate 5) groups)

countPossibleArrangementsMemoized :: Record -> Int
countPossibleArrangementsMemoized (arrangement, groups) = memo M.! (arrangement, groups, 0)
            where memo = M.fromList [((arr, grs, current), solve (arr, grs, current)) | arr <- tails arrangement, grs <- tails groups, current <- [0..maximum groups]]
                  solve ("", [n], current) = if current == n then 1 else 0
                  solve ("", (n:ns), current) = 0
                  solve (arrangement, [], current) = if any (=='#') arrangement || current > 0 then 0 else 1
                  solve ('.':arrangement, groups, 0) = memo M.! (arrangement, groups, 0)
                  solve ('.':arrangement, (n:ns), current) | current == n = memo M.! (arrangement, ns, 0)
                                                           | otherwise = 0
                  solve ('#':arrangement, (n:ns), current) | length ('#':arrangement) < sum ((n-current):ns) = 0
                                                           | current == n = 0
                                                           | otherwise = memo M.! (arrangement, (n:ns), current+1)
                  solve ('?':arrangement, (n:ns), current) | length ('?':arrangement) < sum ((n-current):ns) = 0
                                                           | current == 0 = memo M.! (arrangement, (n:ns), 1) + memo M.! (arrangement, (n:ns), 0)
                                                           | current < n = memo M.! (arrangement, (n:ns), current+1)
                                                           | current == n = memo M.! (arrangement, ns, 0)

main :: IO()
main = do
        file <- readFile "Day 12/12.txt"
        let ls = lines file
            records = map createRecord ls
            possibleArrangements = sum $ map countPossibleArrangements records
            unfoldedRecords = map unfoldRecord records
            possibleUnfoldedArrangements = sum $ map countPossibleArrangementsMemoized unfoldedRecords
        putStrLn $ show possibleArrangements
        putStrLn $ show possibleUnfoldedArrangements