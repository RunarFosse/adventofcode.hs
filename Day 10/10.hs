import Data.Array
import Data.List

-- 1

type Position = (Int, Int)
type Map = Array Position Char

createMap :: [String] -> Map
createMap lines = array ((0, 0), (m-1, n-1)) [((x, y), c) | (y, line) <- zip [0..n-1] lines, (x, c) <- zip [0..m-1] line]
                where (n, m) = (length lines, length $ head lines)

findStart :: Map -> Position
findStart map = head $ [pos | pos <- range(bounds map), map ! pos == 'S']

getViableNeighbours :: Map -> Position -> [Position]
getViableNeighbours map (x, y) | current == 'S' = [(x', y') | (x', y', cs) <- [(x-1, y, ['-', 'L', 'F']), (x+1, y, ['-', 'J', '7']), (x, y-1, ['|', 'F', '7']), (x, y+1, ['|', 'J', 'L'])], x' >= 0 && y' >= 0 && x' <= n && y' <= m && (any (==(map ! (x', y'))) cs)]
                               | current == '|' = [(x', y') | (x', y', cs) <- [(x, y-1, ['|', 'F', '7']), (x, y+1, ['|', 'J', 'L'])], x' >= 0 && y' >= 0 && x' <= n && y' <= m && (map ! (x', y') == 'S' || any (==(map ! (x', y'))) cs)]
                               | current == '-' = [(x', y') | (x', y', cs) <- [(x-1, y, ['-', 'L', 'F']), (x+1, y, ['-', 'J', '7'])], x' >= 0 && y' >= 0 && x' <= n && y' <= m && (map ! (x', y') == 'S' || any (==(map ! (x', y'))) cs)]
                               | current == 'L' = [(x', y') | (x', y', cs) <- [(x+1, y, ['-', 'J', '7']), (x, y-1, ['|', 'F', '7'])], x' >= 0 && y' >= 0 && x' <= n && y' <= m && (map ! (x', y') == 'S' || any (==(map ! (x', y'))) cs)]
                               | current == 'J' = [(x', y') | (x', y', cs) <- [(x-1, y, ['-', 'L', 'F']), (x, y-1, ['|', 'F', '7'])], x' >= 0 && y' >= 0 && x' <= n && y' <= m && (map ! (x', y') == 'S' || any (==(map ! (x', y'))) cs)]
                               | current == '7' = [(x', y') | (x', y', cs) <- [(x-1, y, ['-', 'L', 'F']), (x, y+1, ['|', 'J', 'L'])], x' >= 0 && y' >= 0 && x' <= n && y' <= m && (map ! (x', y') == 'S' || any (==(map ! (x', y'))) cs)]
                               | current == 'F' = [(x', y') | (x', y', cs) <- [(x+1, y, ['-', 'J', '7']), (x, y+1, ['|', 'J', 'L'])], x' >= 0 && y' >= 0 && x' <= n && y' <= m && (map ! (x', y') == 'S' || any (==(map ! (x', y'))) cs)]
                               | otherwise = []
                                where (n, m) = snd $ bounds map
                                      current = map ! (x, y)

followPipes :: Map -> Position -> Position -> Int -> ([Position] -> Position) -> [(Position, Int)]
followPipes map (x, y) last depth startFunc | current == 'S' && last /= (x, y) = []
                                            | current == 'S' && last == (x, y) = ((x, y), depth) : followPipes map (startFunc neighbours) (x, y) (depth + 1) startFunc
                                            | otherwise = ((x, y), depth) : followPipes map ((head . filter (/= last)) neighbours) (x, y) (depth + 1) startFunc
                                            where neighbours = getViableNeighbours map (x, y)
                                                  current = map ! (x, y)

minCombineList :: [Int] -> [Int] -> [Int]
minCombineList [] [] = []
minCombineList (n1:n1s) (n2:n2s) = min n1 n2 : minCombineList n1s n2s

-- 2

encloseMap :: Map -> [Position] -> Map
encloseMap map' edges = map' // (map (\edge -> (edge, if (any (==(map' ! edge)) ['|', 'J', 'L', 'S']) then 'I' else 'P')) edges)

scanLineSearch :: String -> Bool -> Int
scanLineSearch "" _ = 0
scanLineSearch (c:cs) inside | c == 'I' = scanLineSearch cs (not inside)
                             | c == 'P' || not inside = scanLineSearch cs inside
                             | otherwise = 1 + scanLineSearch cs inside

scan :: Map -> [Int]
scan map' = map (\row -> scanLineSearch row False) [[map' ! (x, y) | x <- [0..n]] | y <- [0..m]]
            where (n, m) = snd $ bounds map'

main :: IO()
main = do
        file <- readFile "Day 10/10.txt"
        let ls = lines file
            map' = createMap ls
            start = findStart map'
            search1 = followPipes map' start start 1 head
            search2 = reverse $ followPipes map' start start 1 (head . reverse)
            searchCombined = minCombineList (map snd search1) (map snd search2)
            mapEnclosed = encloseMap map' (map fst search1)
        putStrLn $ show $ maximum searchCombined
        putStrLn $ show $ sum $ scan mapEnclosed