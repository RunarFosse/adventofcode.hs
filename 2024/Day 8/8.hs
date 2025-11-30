import Data.Map as M hiding (foldl, foldr, filter, map)
import Data.List

-- 1

type Antenna = (Int, Int)
type Frequency = Char

parseAntennas :: String -> Map Frequency [Antenna]
parseAntennas file = foldl (\r freq -> M.insert freq (map fst $ filter (\(_, c) -> c == freq) antennas) r) M.empty frequencies
                    where antennas = [((x, y), c) | (y, line) <- zip [0..] (lines file), (x, c) <- zip [0..] line, c /= '.']
                          frequencies = (nub . map snd) antennas

isWithinBounds :: (Int, Int) -> Antenna -> Bool
isWithinBounds (xBound, yBound) (x, y) = x >= 0 && x < xBound && y >= 0 && y < yBound

createAntinodes :: Antenna -> Antenna -> (Int, Int) -> [Antenna]
createAntinodes (x1, y1) (x2, y2) bounds = filter (isWithinBounds bounds) [(x1 - (x2 - x1), y1 - (y2 - y1)), (x2 - (x1 - x2), y2 - (y1 - y2))]

computeAntinodes :: [Antenna] -> (Int, Int) -> [Antenna]
computeAntinodes [] _ = []
computeAntinodes (a1:as) bounds = foldl (\r a2 -> createAntinodes a1 a2 bounds ++ r) [] as ++ computeAntinodes as bounds

-- 2

createCorrectAntinodes :: Antenna -> Antenna -> (Int, Int) -> [Antenna]
createCorrectAntinodes (x1, y1) (x2, y2) bounds = leftAntinodes ++ rightAntinodes
                    where leftAntinodes = takeWhile (isWithinBounds bounds) $ foldr (\multiple r -> (x1 - multiple * (x2 - x1), y1 - multiple * (y2 - y1)):r) [] [0..]
                          rightAntinodes = takeWhile (isWithinBounds bounds) $ foldr (\multiple r -> (x2 - multiple * (x1 - x2), y2 - multiple * (y1 - y2)):r) [] [0..]

computeCorrectAntinodes :: [Antenna] -> (Int, Int) -> [Antenna]
computeCorrectAntinodes [] _ = []
computeCorrectAntinodes (a1:as) bounds = foldl (\r a2 -> createCorrectAntinodes a1 a2 bounds ++ r) [] as ++ computeCorrectAntinodes as bounds

main :: IO()
main = do
        file <- readFile "Day 8/8.txt"
        let (m, n) = ((length . lines) file, (length . head . lines) file)
            antennas = parseAntennas file
            uniqueAntinodes = nub $ foldl (\r as -> computeAntinodes as (n, m) ++ r) [] (elems antennas)
            uniqueCorrectAntinodes = nub $ foldl (\r as -> computeCorrectAntinodes as (n, m) ++ r) [] (elems antennas)
        putStrLn $ show $ length uniqueAntinodes
        putStrLn $ show $ length uniqueCorrectAntinodes