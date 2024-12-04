import Data.Array

-- 1

type WordSearch = Array (Int, Int) Char

createWordSearch :: [String] -> WordSearch
createWordSearch lines = array ((0, 0), (m-1, n-1)) [((i, j), c) | (i, line) <- zip [0..m-1] lines, (j, c) <- zip [0..n-1] line]
                    where (m, n) = (length lines, length $ head lines)

countXMAS :: WordSearch -> Int
countXMAS search = (sum . map countXMASAtIndex . indices) search
                where directions = [(a, b) | a <- [-1..1], b <- [-1..1], (a, b) /= (0, 0)]
                      countXMASAtIndex :: (Int, Int) -> Int
                      countXMASAtIndex position = sum $ map (countOccurence search "XMAS" position) directions

countOccurence :: WordSearch -> String -> (Int, Int) -> (Int, Int) -> Int
countOccurence _ "" _ _ = 1
countOccurence search (c:cs) (i, j) (di, dj) | outsideBounds || search ! (i, j) /= c = 0
                                             | otherwise = countOccurence search cs (i + di, j + dj) (di, dj)
                                    where (height, width) = (snd . bounds) search
                                          outsideBounds = i < 0 || j < 0 || i > height || j > width

-- 2

cartesian :: [a] -> [[a]]
cartesian as = traverse (const as) as

countCrossMAS :: WordSearch -> Int
countCrossMAS search = (sum . map countCrossMASAtIndex . indices) search
                    where words = cartesian ["MAS", "SAM"]
                          countCrossMASAtIndex :: (Int, Int) -> Int
                          countCrossMASAtIndex (i, j) = maximum $ map (\[word1, word2] -> minimum [countOccurence search word1 (i, j-1) (1, 1), countOccurence search word2 (i, j+1) (1, -1)]) words

main :: IO()
main = do
        file <- readFile "Day 4/4.txt"
        let wordSearch = createWordSearch (lines file)
            xmas = countXMAS wordSearch
            crossmas = countCrossMAS wordSearch
        putStrLn $ show xmas
        putStrLn $ show crossmas