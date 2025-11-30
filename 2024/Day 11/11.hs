import Data.Map as M hiding (map, take, drop, foldr)

-- 1

type Stone = Int

createStones :: String -> [Stone]
createStones = map (\number -> read number :: Stone) . words

blink :: Stone -> [Stone]
blink stone | stone == 0 = [1]
            | digits `mod` 2 == 0 = [left, right]
            | otherwise = [stone * 2024]
            where string = show stone
                  digits = length string
                  (left, right) = (read (take (digits `div` 2) string) :: Stone, read (drop (digits `div` 2) string) :: Stone)

blinks :: [Stone] -> Int -> [Stone]
blinks stones 0 = stones
blinks stones n = blinks newStones (n - 1)
                where newStones = foldr ((++) . blink) [] stones

-- 2

type Memo = M.Map (Stone, Int) Int

countAfterblinksMemoized :: Stone -> Int -> Memo -> (Int, Memo)
countAfterblinksMemoized stone n memo | M.member (stone, n) memo = (memo M.! (stone, n), memo)
                                      | otherwise = (result, M.insert (stone, n) result newMemo)
                                      where (result, newMemo) = countAfterblinksMemoizedOver (blink stone) (n - 1) memo

countAfterblinksMemoizedOver :: [Stone] -> Int -> Memo -> (Int, Memo)
countAfterblinksMemoizedOver stones n memo | n == 0 = (length stones, memo)
                                           | otherwise = foldr (\stone (count, memo) -> let result = countAfterblinksMemoized stone n memo in (fst result + count, snd result)) (0, memo) stones


main :: IO()
main = do
        file <- readFile "Day 11/11.txt"
        let stones = createStones file
            numberOfstonesAfter25Blinks = length $ blinks stones 25
            numberOfstonesAfter75Blinks = fst $ countAfterblinksMemoizedOver stones 75 M.empty
        putStrLn $ show numberOfstonesAfter25Blinks
        putStrLn $ show numberOfstonesAfter75Blinks