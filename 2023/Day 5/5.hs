
-- 1

type Map = [(Int, Int, Int)]

createMap :: [String] -> Map
createMap [] = []
createMap ("":lines) = []
createMap (line:lines) = (read dest, read source, read size) : createMap lines
                        where [dest, source, size] = words line

parseInput :: [String] -> [Map]
parseInput [] = []
parseInput ("":lines) = (createMap $ tail lines) : parseInput lines
parseInput (line:lines) = parseInput lines

findDestination :: Map -> Int -> Int
findDestination [] id = id
findDestination ((dest, source, size):map) id | source <= id && source + size > id = (id - source) + dest
                                              | otherwise = findDestination map id

findLocation :: [Map] -> Int -> Int
findLocation [] id = id
findLocation (m:maps) id = findLocation maps $ findDestination m id

-- 2

type Range = (Int, Int)

extractSeedRanges :: [String] -> [Range]
extractSeedRanges [] = []
extractSeedRanges (s1:s2:ss) = (read s1, read s2) : extractSeedRanges ss

splitRange :: Map -> Range -> [Range]
splitRange [] (start, range) = [(start, range)]
splitRange ((dest, source, size):map) (start, range) | source <= start && source + size > start = (newDest, min newSize range) : (if newSize < range then splitRange map (start + newSize, range - newSize) else [])
                                                     | start + range >= source &&  start + range <= source + size = (dest, range + difference) : splitRange map (start, range - (range + difference))
                                                     | otherwise = splitRange map (start, range)
                                                     where difference = start - source
                                                           newDest = difference + dest
                                                           newSize = size - difference

findLocationRanges :: [Map] -> [Range] -> [Range]
findLocationRanges [] ranges = ranges
findLocationRanges (m:maps) ranges = findLocationRanges maps (concat newRanges)
                                     where newRanges = map (splitRange m) ranges

main :: IO()
main = do
        file <- readFile "Day 5/5.txt"
        let ls = lines file
            seeds = map (\n -> read n :: Int) $ (tail . words . head) ls
            maps = parseInput $ tail ls
            locations = map (findLocation maps) seeds
            seedRanges = extractSeedRanges $ (tail . words . head) ls
            locationRanges = findLocationRanges maps seedRanges
        putStrLn $ show $ minimum locations
        putStrLn $ show $ minimum $ map fst locationRanges