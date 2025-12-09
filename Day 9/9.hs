-- 1

type Position = (Int, Int)

readPosition :: String -> Position
readPosition line = (read left :: Int, read right :: Int)
                where left = takeWhile (/= ',') line
                      right = (tail . dropWhile (/= ',')) line

computeRectangle :: Position -> Position -> Int
computeRectangle (x1, y1) (x2, y2) = width * height
                        where width = abs (x2 - x1) + 1
                              height = abs (y2 - y1) + 1

computeLargestRectangle :: [Position] -> Int
computeLargestRectangle [position] = 0
computeLargestRectangle (position:positions) = max rectangle $ computeLargestRectangle positions
                                where rectangle = maximum $ map (computeRectangle position) positions

-- 2

computeBoundaries :: [Position] -> [(Position, Position)]
computeBoundaries positions = zip positions (tail positions ++ [head positions])

isInside :: Position -> Position -> [(Position, Position)] -> Bool
isInside (x1, y1) (x2, y2) boundaries = all isInsideHelper boundaries
                              where isInsideHelper ((bx1, by1), (bx2, by2)) = (max bx1 bx2 <= min x1 x2) || (min bx1 bx2 >= max x1 x2) || (max by1 by2 <= min y1 y2) || (min by1 by2 >= max y1 y2)

computeLargestRedGreenRectangle :: [Position] -> Int
computeLargestRedGreenRectangle positions = maximum rectangles
                    where boundaries = computeBoundaries positions
                          pairs = [(position1, position2) | (i, position1) <- zip [0..] (init positions), position2 <- drop (i + 1) positions]
                          rectangles = [computeRectangle position1 position2 | (position1, position2) <- pairs, isInside position1 position2 boundaries]

main :: IO()
main = do
        file <- readFile "Day 9/9.txt"
        let tiles = map readPosition (lines file)
            largestRectangle = computeLargestRectangle tiles
            largestRedGreenRectangle = computeLargestRedGreenRectangle tiles
        putStrLn $ show $ largestRectangle
        putStrLn $ show $ largestRedGreenRectangle