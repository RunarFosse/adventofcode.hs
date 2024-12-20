import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

-- 1

type Position = (Int, Int)
type Racetrack = Array Position Char

createMap :: String -> Racetrack
createMap file = array ((0, 0), (n - 1, m - 1)) [((x, y), c) | (y, line) <- zip [0..] (lines file), (x, c) <- zip [0..] line]
                where (m, n) = ((length . lines) file, (length . head . lines) file)

type Memo = M.Map Position Int

computePaths :: Racetrack -> [Position] -> S.Set Position -> Memo -> Memo
computePaths _ [] _ memo = memo
computePaths track ((x, y):queue) seen memo | outOfBounds || track ! (x, y) == '#' || S.member (x, y) seen = computePaths track queue seen memo
                                            | M.member (x, y) memo = memo
                                            | track ! (x, y) == 'E' = M.insert (x, y) 0 memo
                                            | otherwise = M.insert (x, y) (1 + nearest) newMemo
                                            where (n, m) = (snd . bounds) track
                                                  outOfBounds = x < 0 || x > n || y < 0 || y > m
                                                  neighbours = [(x + a, y + b) | a <- [-1..1], b <- [-1..1], abs a /= abs b]
                                                  newMemo = computePaths track (queue ++ neighbours) (S.insert (x, y) seen) memo
                                                  nearest = (minimum . map (newMemo M.!) . filter (\position -> M.member position newMemo)) neighbours

race :: Racetrack -> [(Position, Int)] -> S.Set Position -> Memo -> Int -> [Int]
race _ [] _ _ _ = []
race track (((x, y), distance):queue) seen memo cheats | outOfBounds || track ! (x, y) == '#' || S.member (x, y) seen = race track queue seen memo cheats
                                                       | otherwise = map (+distance) (cheat track [((x, y), 0)] S.empty memo cheats) ++ race track (queue ++ neighbours) (S.insert (x, y) seen) memo cheats
                                                       where (n, m) = (snd . bounds) track
                                                             outOfBounds = x < 0 || x > n || y < 0 || y > m
                                                             neighbours = [((x + a, y + b), distance + 1) | a <- [-1..1], b <- [-1..1], abs a /= abs b]

cheat :: Racetrack -> [(Position, Int)] -> S.Set Position -> Memo -> Int -> [Int]
cheat _ [] _ _ _ = []
cheat track (((x, y), distance):queue) seen memo cheats | distance > cheats = []
                                                        | outOfBounds || S.member (x, y) seen = cheat track queue seen memo cheats
                                                        | M.member (x, y) memo = (distance + memo M.! (x, y)) : cheat track (queue ++ neighbours) (S.insert (x, y) seen) memo cheats
                                                        | otherwise = cheat track (queue ++ neighbours) (S.insert (x, y) seen) memo cheats
                                                        where (n, m) = (snd . bounds) track
                                                              outOfBounds = x < 0 || x > n || y < 0 || y > m
                                                              neighbours = [((x + a, y + b), distance + 1) | a <- [-1..1], b <- [-1..1], abs a /= abs b]

-- 2

main :: IO()
main = do
        file <- readFile "Day 20/20.txt"
        let track = createMap file
            (n, m) = (snd . bounds) track
            start = (fst . head . filter ((=='S') . snd) . assocs) track
            memo = computePaths track [start] S.empty M.empty
            boringPath = minimum (race track [(start, 0)] S.empty memo 0)
            cheatingPaths = race track [(start, 0)] S.empty memo 2
            timeSavedByCheating = map (boringPath -) cheatingPaths
            cheatingPathsUpdated = race track [(start, 0)] S.empty memo 20
            timeSavedByCheatingUpdated = map (boringPath -) cheatingPathsUpdated
        putStrLn $ show $ length $ filter (>=100) timeSavedByCheating
        putStrLn $ show $ length $ filter (>=100) timeSavedByCheatingUpdated