import Data.Bits
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M

-- 1

evolveSecret :: Int -> Int
evolveSecret secret = step3
                    where step1 = ((secret * 64) `xor` secret) `mod` 16777216
                          step2 = ((step1 `div` 32) `xor` step1) `mod` 16777216
                          step3 = ((step2 * 2048) `xor` step2) `mod` 16777216

evolve :: Int -> Int -> Int
evolve evolutions secret = foldr (\_ -> evolveSecret) secret [1..evolutions]

-- 2

price :: Int -> Int -> [Int]
price 0 secret = [secret `mod` 10]
price evolutions secret = (evolution `mod` 10) : price (evolutions - 1) evolution
                        where evolution = evolveSecret secret

changes :: [Int] -> [Int]
changes [_] = []
changes (p1:p2:prices) = p2 - p1 : changes (p2 : prices)

possibleBuys :: S.Set [Int] -> [Int] -> M.Map [Int] Int -> M.Map [Int] Int
possibleBuys seen prices bananasMap | length prices < 5 = bananasMap
                                    | S.member sequence seen = possibleBuys seen (tail prices) bananasMap
                                    | otherwise = possibleBuys (S.insert sequence seen) (tail prices) newBananasMap
                                    where sequence = (changes . take 5) prices
                                          bananas = prices !! 4
                                          newBananasMap = M.insertWith (+) sequence bananas bananasMap

main :: IO()
main = do
        file <- readFile "Day 22/22.txt"
        let secrets = (map (\line -> read line :: Int) . lines) file
            newSecrets = map (evolve 2000) secrets
            prices = map (price 2000) secrets
            mostBananas = (maximum . M.elems . foldr (possibleBuys S.empty) M.empty) prices
        putStrLn $ show $ sum newSecrets
        putStrLn $ show $ mostBananas