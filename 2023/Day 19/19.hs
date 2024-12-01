import qualified Data.Map as M

-- 1

type Part = (Int, Int, Int, Int)

type Rule = ((Char, (Int -> Int -> Bool), Int), String)
type Rules = M.Map String [Rule]

splitOn :: Char -> String -> [String]
splitOn c' [] = [""]
splitOn c' (c:cs) | c == c' = "" : splitOn c' cs
                  | otherwise = (c : current) : next
                  where (current:next) = splitOn c' cs

getRating :: Char -> Part -> Int
getRating 'x' (rating, _, _, _) = rating
getRating 'm' (_, rating, _, _) = rating
getRating 'a' (_, _, rating, _) = rating
getRating 's' (_, _, _, rating) = rating

extractOperation :: String -> (Char, (Int -> Int -> Bool), Int)
extractOperation (p:'<':rating) = (p, (<), read rating)
extractOperation (p:'>':rating) = (p, (>), read rating)

extractRule :: String -> Rule
extractRule string | any (==':') string = let [rule, target] = splitOn ':' string in (extractOperation rule, target)
                   | otherwise = ((' ', \_ _ -> True, 0), string)

extractRules :: [String] -> Rules
extractRules lines = M.fromList [(name, map extractRule $ splitOn ',' ((take (length rest - 1)) rest)) | line <- lines, let [name, rest] = splitOn '{' line]

extractParts :: [[String]] -> [Part]
extractParts [] = []
extractParts (part:parts) = (ratings !! 0, ratings !! 1, ratings !! 2, ratings !! 3) : extractParts parts
                          where ratings = map (\string -> read (drop 2 string) :: Int) part

shouldAcceptPart :: Part -> Rules -> String -> Bool
shouldAcceptPart part rules rulename | target == "A" = True
                                     | target == "R" = False
                                     | otherwise = shouldAcceptPart part rules target
                                     where rule = rules M.! rulename
                                           target = (snd . head . dropWhile (\((p, pred, rating), _) -> not $ pred (getRating p part) rating)) rule

assessParts :: [Part] -> Rules -> [Part]
assessParts [] _ = []
assessParts (part:parts) rules | shouldAcceptPart part rules "in" = part : assessParts parts rules
                               | otherwise = assessParts parts rules

getTotalRating :: Part -> Int
getTotalRating (x, m, a, s) = x + m + a + s

-- 2

type Range = (Int, Int)
type PartRange = (Range, Range, Range, Range)

splitPartRange :: PartRange -> (Char, Int) -> (PartRange, PartRange)
splitPartRange ((xmin, xmax), m, a, s) ('x', rating) = (((xmin, rating-1), m, a, s), ((rating, xmax), m, a, s))
splitPartRange (x, (mmin, mmax), a, s) ('m', rating) = ((x, (mmin, rating-1), a, s), (x, (rating, mmax), a, s))
splitPartRange (x, m, (amin, amax), s) ('a', rating) = ((x, m, (amin, rating-1), s), (x, m, (rating, amax), s))
splitPartRange (x, m, a, (smin, smax)) ('s', rating) = ((x, m, a, (smin, rating-1)), (x, m, a, (rating, smax)))

extractPartRangesFromSingleRule :: PartRange -> (Char, (Int -> Int -> Bool), Int) -> (PartRange, PartRange)
extractPartRangesFromSingleRule range (part, pred, rating) | pred 0 0 = (range, ((0,0),(0,0),(0,0),(0,0)))
                                                           | pred 1 2 = splitPartRange range (part, rating)
                                                           | otherwise = (gt, lte)
                                                            where (lte, gt) = splitPartRange range (part, rating+1)

calculateRulePartRanges :: PartRange -> [Rule] -> ([PartRange], [(String, PartRange)])
calculateRulePartRanges _ [] = ([], [])
calculateRulePartRanges range ((singleRule, target):rule) | target == "A" = (fulfilledRange:accepted, continued)
                                                          | target == "R" = (accepted, continued)
                                                          | otherwise = (accepted, (target, fulfilledRange):continued)
                                                          where (fulfilledRange, notFulfilledRange) = extractPartRangesFromSingleRule range singleRule
                                                                (accepted, continued) = calculateRulePartRanges notFulfilledRange rule

extractAcceptedPartRanges :: PartRange -> Rules -> String -> [PartRange]
extractAcceptedPartRanges range rules rulename = accepted ++ concat (map (\(target, range) -> extractAcceptedPartRanges range rules target) continued)
                                                where rule = rules M.! rulename
                                                      (accepted, continued) = calculateRulePartRanges range rule

computeDistinctAcceptedCombinations :: PartRange -> Int
computeDistinctAcceptedCombinations ((xmin, xmax), (mmin, mmax), (amin, amax), (smin, smax)) = (xmax-xmin+1) * (mmax-mmin+1) * (amax-amin+1) * (smax-smin+1)

main :: IO()
main = do
        file <- readFile "Day 19/19.txt"
        let ls = lines file
            (rulesLines, partsLines) = (takeWhile (/="") ls, tail $ dropWhile (/="") ls)
            rules = extractRules rulesLines
            parts = extractParts $ map (splitOn ',' . drop 1 . reverse . drop 1 . reverse) partsLines
            acceptedParts = assessParts parts rules
            sumAcceptedRatings = sum $ map getTotalRating acceptedParts
            possiblePartRanges = ((1, 4000), (1, 4000), (1, 4000), (1, 4000))
            acceptedPartRanges = extractAcceptedPartRanges possiblePartRanges rules "in"
            totalDistinctAcceptedCombinations = sum $ map computeDistinctAcceptedCombinations acceptedPartRanges
        putStrLn $ show sumAcceptedRatings
        putStrLn $ show totalDistinctAcceptedCombinations