import Data.Array
import Data.Char

-- 1

type Engine = Array Int Char


createEngine :: [String] -> (Int, Int) -> Engine
createEngine lines (m, n) = array (0, m*n-1) [(x+y*n, c) | (y, line) <- zip [0..m-1] lines, (x, c) <- zip [0..n-1] line]

isAdjacent :: Engine -> (Int, Int) -> (Int, Int) -> Bool
isAdjacent engine dims@(m, n) (x, y) = any (==True) [not (isDigit symbol) && symbol /= '.' ||
                                            isDigit symbol && (x' > x && y' == y) && isAdjacent engine dims (x', y') | 
                                                                x' <- [x-1..x+1], 
                                                                y' <- [y-1..y+1], 
                                                                let symbol = engine ! (x'+y'*n),
                                                                x' >= 0 && x' < n && y' >= 0 && y' < m && (x', y') /= (x, y)] 

extractPartNumber :: Engine -> (Int, Int) -> (Int, Int) -> (String, (Int, Int))
extractPartNumber engine dims@(m, n) pos@(x, y) | pos == (n, y) = ("", (0, y+1))
                                                | not $ isDigit symbol = ("", (x, y))
                                                | otherwise = (symbol : number, nextPos)
                                                  where symbol = engine ! (x+y*n)
                                                        (number, nextPos) = extractPartNumber engine dims (x+1, y)

findPartNumbers :: Engine -> (Int, Int) -> (Int, Int) -> [Int]
findPartNumbers engine dims@(m, n) pos@(x, y) | y >= m = []
                                              | x == n = findPartNumbers engine dims (0, y+1)
                                              | isDigit (engine ! (x+y*n)) && isAdjacent engine dims pos = partNumber : (findPartNumbers engine dims nextPos)
                                              | otherwise = findPartNumbers engine dims (x+1, y)
                                                where (number, nextPos) = extractPartNumber engine dims pos
                                                      partNumber = read number :: Int

-- 2

countDifferentNumbers :: [[String]] -> Int
countDifferentNumbers [] = 0
countDifferentNumbers ([]:nums) = countDifferentNumbers nums
countDifferentNumbers (("":ss):nums) = countDifferentNumbers (ss:nums)
countDifferentNumbers ((s:ss):nums) = 1 + countDifferentNumbers (ss:nums)
                                                        
extractFullNumbers :: Engine -> (Int, Int) -> (Int, Int) -> [String]
extractFullNumbers engine dims@(m, n) (x, y) | isDigit symbol = [reverse leftNumber ++ (symbol : rightNumber)]
                                             | otherwise = [reverse leftNumber, rightNumber]
                                             where symbol = engine ! (x+y*n)
                                                   left = array (0, x-1) [(i, engine ! (x'+y*n)) | (i, x') <- zip [0..x-1] (reverse [0..x-1])]
                                                   right = array (0, n-x-2) [(i, engine ! (x'+y*n)) | (i, x') <- zip [0..n-x-2] [x+1..n-1]]
                                                   (leftNumber, _) = extractPartNumber left (1, x) (0, 0)
                                                   (rightNumber, _) = extractPartNumber right (1, n-x-1) (0,0)

isGear :: Engine -> (Int, Int) -> (Int, Int) -> Bool
isGear engine dims@(m, n) (x, y) = (engine ! (x+y*n) == '*') && numberOfSurroundingNumbers == 2
                                 where numberOfSurroundingNumbers = countDifferentNumbers surroundingNumbers
                                       surroundingNumbers = map (extractFullNumbers engine dims) [(x, y') |
                                                                y' <- [y-1..y+1], 
                                                                y' >= 0 && y' < m]


calculateGearRatio :: [[String]] -> Int
calculateGearRatio [] = 1
calculateGearRatio ([]:nums) = calculateGearRatio nums
calculateGearRatio (("":ss):nums) = calculateGearRatio (ss:nums)
calculateGearRatio ((s:ss):nums) = (read s :: Int) * calculateGearRatio (ss:nums)

findGearRatios :: Engine -> (Int, Int) -> (Int, Int) -> [Int]
findGearRatios engine dims@(m, n) pos@(x, y) | x == n = if y == m-1 then [] else findGearRatios engine dims (0, y+1)
                                             | isGear engine dims pos = (calculateGearRatio surroundingNumbers) : findGearRatios engine dims (x+1, y)
                                             | otherwise = findGearRatios engine dims (x+1, y)
                                             where surroundingNumbers = map (extractFullNumbers engine dims) [(x, y') |
                                                                y' <- [y-1..y+1], 
                                                                y' >= 0 && y' < m]

main :: IO()
main = do
    file <- readFile "Day 3/3.txt"
    let ls = lines file
        dimensions@(m, n) = (length ls, length $ head ls)
        engine = createEngine ls dimensions
        partNumbers = findPartNumbers engine dimensions (0,0)
        gearRatios = findGearRatios engine dimensions (0,0)
        (x, y) = (55, 2)
    putStrLn $ show $ sum partNumbers
    putStrLn $ show $ sum gearRatios