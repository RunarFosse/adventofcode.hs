import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import PriorityQueue

-- 1

type Position = (Int, Int, Int)
newtype Brick = Brick (Int, Position, Position) deriving (Show, Eq)

instance Ord Brick where
    compare (Brick (_,(_,_,z1),_)) (Brick (_,(_,_,z2),_)) = compare z1 z2

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c' (c:cs) | c' == c = "" : splitOn c' cs
                  | otherwise = (c:current) : rest
                   where (current:rest) = splitOn c' cs

parseBricks :: [String] -> [Brick]
parseBricks [] = []
parseBricks (line:lines) = (Brick (length lines, (x1, y1, z1), (x2, y2, z2))) : parseBricks lines
                          where [end1, end2] = splitOn '~' line
                                [x1, y1, z1] = (map read . splitOn ',') end1
                                [x2, y2, z2] = (map read . splitOn ',') end2

type SupportMap = M.Map Position (Int, [Int])

dropBrick :: Brick -> Int -> Brick
dropBrick (Brick (id, (x1, y1 ,z1), (x2, y2, z2))) spaces = Brick (id, (x1, y1, z1-spaces), (x2, y2, z2-spaces))

brickRange :: Brick -> [Position]
brickRange (Brick (_, (x1, y1 ,z1), (x2, y2, z2))) | (x1,y1) == (x2,y2) = [(x1,y1,z) | z <- [z1..z2]]
                                                   | (x1,z1) == (x2,z2) = [(x1,y,z1) | y <- [y1..y2]]
                                                   | (y1,z1) == (y2,z2) = [(x,y1,z1) | x <- [x1..x2]]

dropPositions :: Brick -> [Position]
dropPositions brick = filter (\ (_,_,z) -> z == lowestZ) dropRange
                    where dropRange = brickRange $ dropBrick brick 1
                          lowestZ = (minimum . map (\(_,_,z) -> z)) dropRange

supportsPositions :: Brick -> [Position]
supportsPositions brick = filter (\(_,_,z) -> z == highestZ) supportsRange
                    where supportsRange = brickRange $ dropBrick brick (-1)
                          highestZ = (maximum . map (\(_,_,z) -> z)) supportsRange

dropBricks :: [Brick] -> SupportMap -> ([Brick], SupportMap)
dropBricks [] supports = ([], supports)
dropBricks (b@(Brick (id,(_,_,z),_)):bs) supports | length occupiedDropPositions == 0 && z > 1 = dropBricks ((dropBrick b 1):bs) supports
                                                  | otherwise = (b:bricks, finalSupports)
                                                   where occupiedDropPositions = filter (\pos -> M.member pos supports) (dropPositions b)
                                                         bricksUnderneath = map (\pos -> fst $ supports M.! pos) occupiedDropPositions
                                                         updatedSupports = foldr (\pos supp -> M.insert pos (id, nub bricksUnderneath) supp) supports $ brickRange b
                                                         (bricks, finalSupports) = dropBricks bs updatedSupports

getSoleSupports :: Brick -> SupportMap -> [Int]
getSoleSupports brick supports = soleSupportedBricks
                                where supportedPositions = supportsPositions brick
                                      supportedBricks = (map (supports M.!) . filter (\pos -> M.member pos supports)) supportedPositions
                                      soleSupportedBricks = (map fst . filter (\b -> (length . snd) b == 1)) supportedBricks

-- 2

type BrickMap = M.Map Int Brick
type Disintegrated = S.Set Int

getSupports :: Brick -> SupportMap -> [Int]
getSupports brick supports = map fst supportedBricks
                            where supportedPositions = supportsPositions brick
                                  supportedBricks = (map (supports M.!) . filter (\pos -> M.member pos supports)) supportedPositions

willFall :: Int -> SupportMap -> BrickMap -> Disintegrated -> Bool
willFall brick supports bricks disintegrated = all (\id -> S.member id disintegrated) (snd $ supports M.! pos)
                                             where (Brick (_, pos, _)) = bricks M.! brick

getDisintegrateChainReaction :: Heap Brick -> SupportMap -> BrickMap -> Disintegrated -> [Int]
getDisintegrateChainReaction queue supports bricks disintegrated | rank queue == 0 = []
                                                                 | length supportedWhichFall == 0 = getDisintegrateChainReaction restqueue supports bricks disintegrated
                                                                 | otherwise = supportedWhichFall ++ getDisintegrateChainReaction fallqueue supports bricks updatedDisintegrated
                                                                 where Just ((Brick (id, _, _)), restqueue) = extractMin queue
                                                                       supported = nub $ getSupports (bricks M.! id) supports
                                                                       supportedWhichFall = filter (\id -> (willFall id supports bricks disintegrated) && (S.notMember id disintegrated)) supported
                                                                       updatedDisintegrated = foldr (\id dis -> S.insert id dis) disintegrated supportedWhichFall
                                                                       fallqueue = merge (fromList $ map (\id -> bricks M.! id) supportedWhichFall) restqueue

main :: IO()
main = do
        file <- readFile "Day 22/22.txt"
        let bricks = (sort . parseBricks . lines) file
            (dropped, supportMap) = dropBricks bricks M.empty
            brickMap = foldr (\brick@(Brick (id,_,_)) m -> M.insert id brick m) M.empty dropped
            disintegratable = filter (\brick -> length (getSoleSupports brick supportMap) == 0) dropped
            chainReactors = map (\(Brick (id,_,_)) -> id) $ filter (\brick -> length (getSoleSupports brick supportMap) /= 0) dropped
            chainReactions = map (length . (\brick -> getDisintegrateChainReaction (fromList [brickMap M.! brick]) supportMap brickMap (S.fromList [brick]))) chainReactors
        putStrLn $ show $ length disintegratable
        putStrLn $ show $ sum chainReactions