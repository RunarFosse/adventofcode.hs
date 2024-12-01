import Data.List

-- 1

type Landscape = [String]

createLandscapes :: [String] -> [Landscape]
createLandscapes [] = [[]]
createLandscapes (line:lines) | line == "" = [] : createLandscapes lines
                              | otherwise = (line : landscape) : otherLandscapes
                              where (landscape:otherLandscapes) = createLandscapes lines

verifyMirrorLocation :: Landscape -> Landscape -> Bool
verifyMirrorLocation [] _ = True
verifyMirrorLocation _ [] = True
verifyMirrorLocation (row1:rows1) (row2:rows2) | row1 == row2 = verifyMirrorLocation rows1 rows2
                                               | otherwise = False

findMirrorLocation :: Landscape -> Landscape -> Int -> Maybe Int
findMirrorLocation [row] _ _  = Nothing
findMirrorLocation (row1:row2:rows) landscape i | row1 == row2 && verifyMirrorLocation ((reverse . take i) landscape) (drop i landscape) = Just i
                                                | otherwise = findMirrorLocation (row2:rows) landscape (i+1)

extractMirrorLocation :: Landscape -> Int
extractMirrorLocation landscape = case findMirrorLocation landscape landscape 1 of
                                        Just i -> i * 100
                                        Nothing -> case findMirrorLocation (transpose landscape) (transpose landscape) 1 of
                                                    Just i -> i
                                                    Nothing -> error "No mirror"

-- 2

stringDifference :: String -> String -> Int
stringDifference "" "" = 0
stringDifference (c1:s1) (c2:s2) | c1 == c2 = stringDifference s1 s2
                                 | otherwise = 1 + stringDifference s1 s2

verifySmudgedMirrorLocation :: Landscape -> Landscape -> Bool -> Bool
verifySmudgedMirrorLocation [] _ fixed = fixed
verifySmudgedMirrorLocation _ [] fixed = fixed
verifySmudgedMirrorLocation (row1:rows1) (row2:rows2) fixed | row1 == row2 = verifySmudgedMirrorLocation rows1 rows2 fixed
                                                            | stringDifference row1 row2 == 1 && not fixed = verifySmudgedMirrorLocation rows1 rows2 True
                                                            | otherwise = False

findSmudgedMirrorLocation :: Landscape -> Landscape -> Int -> Maybe Int
findSmudgedMirrorLocation [row] _ _  = Nothing
findSmudgedMirrorLocation (row1:row2:rows) landscape i | (row1 == row2 || stringDifference row1 row2 == 1) && verifySmudgedMirrorLocation ((reverse . take i) landscape) (drop i landscape) False = Just i
                                                       | otherwise = findSmudgedMirrorLocation (row2:rows) landscape (i+1)

extractSmudgedMirrorLocation :: Landscape -> Int
extractSmudgedMirrorLocation landscape = case findSmudgedMirrorLocation landscape landscape 1 of
                                        Just i -> i * 100
                                        Nothing -> case findSmudgedMirrorLocation (transpose landscape) (transpose landscape) 1 of
                                                    Just i -> i
                                                    Nothing -> error "No mirror"

main :: IO()
main = do
        file <- readFile "Day 13/13.txt"
        let landscapes = createLandscapes $ lines file
            mirrorLocations = map extractMirrorLocation landscapes
            smudgedMirrorLocations = map extractSmudgedMirrorLocation landscapes
        putStrLn $ show $ sum mirrorLocations
        putStrLn $ show $ sum smudgedMirrorLocations