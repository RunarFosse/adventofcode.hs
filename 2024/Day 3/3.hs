import Data.Char

-- 1

isMultiplication :: String -> Maybe (Int, Int)
isMultiplication string | prefix == "mul(" && separator == ',' && suffix == ')' = Just (read n1 :: Int, read n2 :: Int)
                        | otherwise = Nothing
                        where prefix = take 4 string
                              n1 = (takeWhile isDigit . drop 4) string
                              separator = (head . drop (4 + length n1)) string
                              n2 = (takeWhile isDigit . drop (5 + length n1)) string
                              suffix = (head . drop (5 + length n1 + length n2)) string

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil predicate (a:as) | predicate a = [a]
                           | otherwise = a : takeUntil predicate as

readMemory :: String -> [(Int, Int)]
readMemory [] = []
readMemory memory = case multiplication of 
                        Just numbers -> numbers : readMemory remainingMemory
                        Nothing -> readMemory remainingMemory
                where string = takeUntil (== ')') memory
                      multiplication = isMultiplication string
                      remainingMemory = dropWhile (/= 'm') $ tail memory

multiplyAndSum :: [(Int, Int)] -> Int
multiplyAndSum = foldl (\r mul -> r + fst mul * snd mul) 0

-- 2

readConditionalMemory :: Bool -> String -> [(Int, Int)]
readConditionalMemory _ [] = []
readConditionalMemory enabled memory | not enabled = readConditionalMemory (string == "do()") remainingMemory
                                     | otherwise = case multiplication of 
                                                   Just numbers -> numbers : readConditionalMemory True remainingMemory
                                                   Nothing -> readConditionalMemory (string /= "don't()") remainingMemory
                                    where string = takeUntil (== ')') memory
                                          multiplication = isMultiplication string
                                          remainingMemory = dropWhile (\c -> not $ elem c ['d', 'm']) $ tail memory

main :: IO()
main = do
        file <- readFile "Day 3/3.txt"
        let multiplications = readMemory file
            conditionalMultiplications = readConditionalMemory True file
        putStrLn $ show $ multiplyAndSum multiplications
        putStrLn $ show $ multiplyAndSum conditionalMultiplications
