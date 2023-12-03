import Data.Char

-- 1

data Color = Red Int | Green Int | Blue Int deriving Show

extractNumber :: String -> String
extractNumber [] = ""
extractNumber (c:cs) | isDigit c = c : extractNumber cs
                     | otherwise = ""

extractColor :: (String, String) -> Color
extractColor (count, color) = case color of
                              "red" -> Red $ read count
                              "green" -> Green $ read count
                              "blue" -> Blue $ read count


extractColors :: [String] -> [[Color]]
extractColors [] = []
extractColors (count:colors:ss) | seperator == ';' = [extractColor (count, color)] : nextColors
                                | otherwise = [extractColor (count, color) : (head nextColors)] ++ (tail nextColors)
                                         where color = reverse $ tail $ reverse colors
                                               seperator = head $ reverse colors
                                               nextColors = extractColors ss

parseGame :: String -> (Int, [[Color]])
parseGame game = (id, colors)
                where tokens = words (game ++ ";")
                      id = read (extractNumber $ tokens !! 1) :: Int
                      colors = extractColors $ drop 2 tokens

isPossible :: [Color] -> Bool
isPossible [] = True
isPossible ((Red count):colors) = if count <= 12 then isPossible colors else False
isPossible ((Green count):colors) = if count <= 13 then isPossible colors else False
isPossible ((Blue count):colors) = if count <= 14 then isPossible colors else False

sumPossibleGameIds :: [(Int, [[Color]])] -> Int
sumPossibleGameIds [] = 0
sumPossibleGameIds ((id, colors):games) | all (==True) $ map isPossible colors = id + sumPossibleGameIds games
                                         | otherwise = sumPossibleGameIds games

-- 2

sumColorCounts :: Int -> Int -> Int -> [Color] -> (Color, Color, Color)
sumColorCounts reds greens blues [] = (Red reds, Green greens, Blue blues)
sumColorCounts reds greens blues (color:colors) = case color of
                                                    Red count -> sumColorCounts (max reds count) greens blues colors
                                                    Green count -> sumColorCounts reds (max greens count) blues colors
                                                    Blue count -> sumColorCounts reds greens (max blues count) colors

colorPower :: (Color, Color, Color) -> Int
colorPower (Red reds, Green greens, Blue blues) = reds * greens * blues

main :: IO()
main = do
    file <- readFile "Day 2/2.txt"
    let ls = lines file
        games = map parseGame ls
        colorCounts = map (sumColorCounts 0 0 0) $ map (foldr (++) []) (map snd games)
    putStrLn $ show $ sumPossibleGameIds games
    putStrLn $ show $ sum $ map colorPower colorCounts
