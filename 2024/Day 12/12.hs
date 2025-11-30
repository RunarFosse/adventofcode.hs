import Data.Array
import Data.Set as S hiding (filter, foldr)

-- 1

type Position = (Int, Int)
type Map = Array Position Char

type LandInformation = (Char, Int, Int, Int)

createMap :: String -> Map
createMap file = array ((0, 0), (n-1, m-1)) [((x, y), c) | (y, line) <- zip [0..] (lines file), (x, c) <- zip [0..] line]
                where (m, n) = ((length . lines) file, (length . head . lines) file)

searchLand :: Map -> [Position] -> S.Set Position -> LandInformation -> (LandInformation, S.Set Position)
searchLand _ [] seen info = (info, seen)
searchLand map ((x, y):poss) seen info@(plant, area, perimeter, corners) | S.member (x, y) seen = searchLand map poss seen info
                                                                         | otherwise = searchLand map (poss ++ filter isNeighbour possibleNeighbours) (S.insert (x, y) seen) (plant, area + 1, perimeter + (length . filter (not . isNeighbour)) possibleNeighbours, corners + length currentCorners)
                                                                where (n, m) = (snd . bounds) map
                                                                      inBounds (x, y) = x >= 0 && x <= n && y >= 0 && y <= m
                                                                      isNeighbour :: Position -> Bool
                                                                      isNeighbour pos = inBounds pos && map ! pos == plant
                                                                      possibleNeighbours = [(x + a, y + b) | a <- [-1..1], b <- [-1..1], abs a /= abs b]
                                                                      possibleCorners = [((x + a, y), (x, y + b), (x + a, y + b)) | a <- [-1..1], b <- [-1..1], (a, b) /= (0, 0)]
                                                                      currentCorners = filter (\(side1, side2, middle) -> (isNeighbour side1 && isNeighbour side2 && (not . isNeighbour) middle) || ((not . isNeighbour) side1 && (not . isNeighbour) side2)) possibleCorners


computeLandInformation :: Map -> Position -> S.Set Position -> [LandInformation]
computeLandInformation map (x, y) seen | (x, y) == (snd . bounds) map = []
                                       | S.member (x, y) seen = computeLandInformation map newPosition newSeen
                                       | otherwise = info : computeLandInformation map newPosition newSeen
                                       where (info, newSeen) = searchLand map [(x, y)] seen (map ! (x, y), 0, 0, 0)
                                             newPosition = if x == (fst . snd . bounds) map then (0, y + 1) else (x + 1, y)

computePrice :: LandInformation -> Int
computePrice (_, area, perimeter, _) = area * perimeter

-- 2

computeSidePrice :: LandInformation -> Int
computeSidePrice (_, area, _, corners) = area * corners

main :: IO()
main = do
        file <- readFile "Day 12/12.txt"
        let map = createMap file
            landInformation = computeLandInformation map (0, 0) S.empty
            totalPrice = foldr ((+) . computePrice) 0 landInformation
            totalSidePrice = foldr ((+) . computeSidePrice) 0 landInformation
        putStrLn $ show totalPrice
        putStrLn $ show totalSidePrice