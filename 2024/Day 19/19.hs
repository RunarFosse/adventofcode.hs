import qualified Data.Map as M

-- 1

type Towel = String

parseInput :: String -> ([Towel], [Towel])
parseInput file = ((map (filter (/=',')) . words . head . lines) file, (drop 2 . lines) file)

type Memo = M.Map Towel Bool

canBeDisplayed :: [Towel] -> Memo -> Towel -> (Bool, Memo)
canBeDisplayed _ memo "" = (True, memo)
canBeDisplayed towels memo design | M.member design memo = (memo M.! design, memo)
                                  | otherwise = (possible, M.insert design possible newMemo)
                                   where matches = foldr (\towel matches -> if towel == take (length towel) design then drop (length towel) design:matches else matches) [] towels
                                         (possible, newMemo) = foldr (\design (possible, memo) -> let result = canBeDisplayed towels memo design in (possible || fst result, M.union memo (snd result))) (False, memo) matches
                                   
-- 2

type CountMemo = M.Map Towel Int

countDisplays :: [Towel] -> CountMemo -> Towel -> (Int, CountMemo)
countDisplays _ memo "" = (1, memo)
countDisplays towels memo design | M.member design memo = (memo M.! design, memo)
                                 | otherwise = (counts, M.insert design counts newMemo)
                                 where matches = foldr (\towel matches -> if towel == take (length towel) design then drop (length towel) design:matches else matches) [] towels
                                       (counts, newMemo) = foldr (\design (counts, memo) -> let result = countDisplays towels memo design in (counts + fst result, M.union memo (snd result))) (0, memo) matches

main :: IO()
main = do
        file <- readFile "Day 19/19.txt"
        let (towels, designs) = parseInput file
            possibleDesigns = filter (fst . canBeDisplayed towels M.empty) designs
            possibleDesignCounts = (sum . map (fst . countDisplays towels M.empty)) designs
        putStrLn $ show $ length $ possibleDesigns
        putStrLn $ show $ possibleDesignCounts