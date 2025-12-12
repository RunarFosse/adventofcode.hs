import Data.Char

-- 1

type Position = (Int, Int)
type Present = [Position]
type Region = (Position, [Int])

parsePresents :: [String] -> ([Present], [String])
parsePresents file | elem 'x' (head file) = ([], file)
                   | otherwise = (present:presents, remainder)
                where present = [(x, y) | (y, line) <- zip [0..] ((take 3 . drop 1) file), (x, c) <- zip [0..] line, c == '#']
                      (presents, remainder) = parsePresents (drop 5 file)

parseRegion :: String -> Region
parseRegion line = (size, map read presents)
            where (dimensions:presents) = words line
                  size = ((read . takeWhile isDigit) dimensions, (read . takeWhile isDigit . tail . dropWhile isDigit) dimensions)

fitPresentsNaive :: [Present] -> Region -> Bool
fitPresentsNaive presents (size, counts) = presentsArea < regionArea
                            where regionArea = fst size * snd size
                                  presentsArea = sum [length present * count | (present, count) <- zip presents counts]

-- 2

-- Merry Christmas!

main :: IO()
main = do
        file <- readFile "Day 12/12.txt"
        let (presents, remainder) = parsePresents (lines file)
            regions = map parseRegion remainder
            fittingRegions = filter (fitPresentsNaive presents) regions
        putStrLn $ show $ length fittingRegions