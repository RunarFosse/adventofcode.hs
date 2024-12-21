import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

-- 1

type State = (Char, Char, Char)
type Graph = M.Map State [State]

graph :: Graph
graph = M.fromList [let state = (c1, c2, c3) in (state, neighbours state) | c1 <- directionalKeypad, c2 <- directionalKeypad, c3 <- numericalKeypad]
      where directionalKeypad = ['^', 'v', '<', '>', 'A']
            numericalKeypad = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'A']

indexOfDirection :: Char -> Int
indexOfDirection direction = (fromJust . findIndex (==direction)) ['<', '^', '>', 'v', 'A']

directionalNeighbour :: Char -> Char -> Maybe Char
directionalNeighbour '^' direction = [Nothing, Nothing, Just 'A', Just 'v', Nothing] !! indexOfDirection direction
directionalNeighbour 'v' direction = [Just '<', Just '^', Just '>', Nothing, Nothing] !! indexOfDirection direction
directionalNeighbour '<' direction = [Nothing, Nothing, Just 'v', Nothing, Nothing] !! indexOfDirection direction
directionalNeighbour '>' direction = [Just 'v', Just 'A', Nothing, Nothing, Nothing] !! indexOfDirection direction
directionalNeighbour 'A' direction = [Just '^', Nothing, Nothing, Just '>', Nothing] !! indexOfDirection direction

numericalNeighbour :: Char -> Char -> Maybe Char
numericalNeighbour '1' direction = [Nothing, Just '4', Just '2', Nothing, Nothing] !! indexOfDirection direction
numericalNeighbour '2' direction = [Just '1', Just '5', Just '3', Just '0', Nothing] !! indexOfDirection direction
numericalNeighbour '3' direction = [Just '2', Just '6', Nothing, Just 'A', Nothing] !! indexOfDirection direction
numericalNeighbour '4' direction = [Nothing, Just '7', Just '1', Just '4', Nothing] !! indexOfDirection direction
numericalNeighbour '5' direction = [Just '4', Just '8', Just '6', Just '2', Nothing] !! indexOfDirection direction
numericalNeighbour '6' direction = [Just '5', Just '9', Nothing, Just '3', Nothing] !! indexOfDirection direction
numericalNeighbour '7' direction = [Nothing, Nothing, Just '8', Just '4', Nothing] !! indexOfDirection direction
numericalNeighbour '8' direction = [Just '7', Nothing, Just '9', Just '5', Nothing] !! indexOfDirection direction
numericalNeighbour '9' direction = [Just '8', Nothing, Nothing, Just '6', Nothing] !! indexOfDirection direction
numericalNeighbour '0' direction = [Nothing, Just '2', Just 'A', Nothing, Nothing] !! indexOfDirection direction
numericalNeighbour 'A' direction = [Just '0', Just '3', Nothing, Nothing, Nothing] !! indexOfDirection direction

neighbours :: State -> [State]
neighbours (c1, c2, c3) | c1 == 'A' = case numericalNeighbour c3 c2 of
                                            Just c -> (c1, c2, c) : ourControls
                                            Nothing -> ourControls
                        | otherwise = case directionalNeighbour c2 c1 of
                                            Just c -> (c1, c, c3) : ourControls
                                            Nothing -> ourControls
                            where allDirectionalNeighbours = (map fromJust . filter isJust . map (directionalNeighbour c1)) ['^', 'v', '<', '>']
                                  ourControls = [(c, c2, c3) | c <- allDirectionalNeighbours]

shortestPath :: [(State, Int)] -> S.Set State -> String -> Int
shortestPath _ _ [] = 0
shortestPath ((state, distance):queue) seen code | S.member state seen = shortestPath queue seen code
                                                 | state == ('A', 'A', head code) = (1 + distance) + shortestPath [(state, 0)] S.empty (tail code)
                                                 | otherwise = shortestPath (queue ++ map (\state -> (state, distance + 1)) (graph M.! state)) (S.insert state seen) code

computeComplexity :: (String, Int) -> Int
computeComplexity (code, sequence) = sequence * read (init code) :: Int 

-- 2

type Memo = M.Map (Int, String) Int

solve ::  Int -> Int -> Memo -> String -> (Int, Memo)
solve robot robots memo code | robot == robots = (length code, memo)
                             | M.member (robot, code) memo = (memo M.! (robot, code), memo)
                             | otherwise = (result, M.insert (robot, code) result newMemo)
                             where computePaths = if robot == 0 then computeNumericalPaths else computeDirectionalPaths
                                   (result, _, newMemo) = foldl (\(steps, from, memo) to -> let paths = computePaths [(from, "")] to in (\(results, memo) -> (steps + minimum results, to, memo)) (foldr (\path (results, memo) -> (\(r, memo) -> (r:results, memo)) (solve (robot + 1) robots memo path)) ([], memo) paths)) (0, 'A', memo) code

computeDirectionalPaths :: [(Char, String)] -> Char -> [String]
computeDirectionalPaths [] _ = []
computeDirectionalPaths ((from, path):queue) to | from == to = map ((++"A") . snd) (filter ((==to) . fst) ((from, path):queue))
                                                | otherwise = computeDirectionalPaths (queue ++ map (\(direction, c) -> (c, path ++ [direction])) allNeighbours) to
                        where allNeighbours = (map (\(direction, Just c) -> (direction, c)) . filter (isJust . snd) . map (\direction -> (direction, directionalNeighbour from direction))) ['<', '^', 'v', '>']

computeNumericalPaths :: [(Char, String)] -> Char -> [String]
computeNumericalPaths ((from, path):queue) to | from == to = map ((++"A") . snd) (filter ((==to) . fst) ((from, path):queue))
                                              | otherwise = computeNumericalPaths (queue ++ map (\(direction, c) -> (c, path ++ [direction])) allNeighbours) to
                        where allNeighbours = (map (\(direction, Just c) -> (direction, c)) . filter (isJust . snd) . map (\direction -> (direction, numericalNeighbour from direction))) ['<', '^', 'v', '>']

main :: IO()
main = do
        file <- readFile "Day 21/21.txt"
        let codes = lines file
            sequenceLengths = map (shortestPath [(('A', 'A', 'A'), 0)] S.empty) codes
            complexities = map computeComplexity (zip codes sequenceLengths)
            largeSequenceLengths = map (fst . solve 0 26 M.empty) codes
            complexitiesLarge = map computeComplexity (zip codes largeSequenceLengths)
        putStrLn $ show $ sum complexities
        putStrLn $ show $ sum complexitiesLarge