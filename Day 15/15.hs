import Data.Char
import Data.List

-- 1

splitOn :: String -> Char -> [String]
splitOn "" _ = [""]
splitOn (c:cs) c' | c == c' = ("") : splitOn cs c'
                  | otherwise = (c:current) : rest
                  where (current:rest) = splitOn cs c'

hash :: Int -> String -> Int
hash current "" = current
hash current (c:cs) = hash newhash cs
                    where newhash = ((ord c + current) * 17) `rem` 256

-- 2

data Operation = Add String Int | Remove String deriving (Show)

type Lens = (String, Int)

createOperations :: [String] -> [Operation]
createOperations [] = []
createOperations (line:lines) | any (=='=') line = let [label, focal] = splitOn line '=' in (Add label (read focal)) : createOperations lines
                              | otherwise = let [label, _] = splitOn line '-' in (Remove label) : createOperations lines

replaceLens :: Lens -> [Lens] -> [Lens]
replaceLens lens@(label, focal) ((l, f):box) | label == l = lens : box
                                             | otherwise = (l, f) : replaceLens lens box

insertLens :: Lens -> [[Lens]] -> Int -> [[Lens]]
insertLens lens@(label, focal) boxes index | any (\l -> fst l == label) box = before ++ replaceLens lens box : after
                                      | otherwise = before ++ (box ++ [lens]) : after
                                      where (before, (box:after)) = splitAt index boxes

removeLens :: String -> [[Lens]] -> Int -> [[Lens]]
removeLens label boxes index | any (\l -> fst l == label) box = before ++ (filter (\l -> fst l /= label) box) : after
                             | otherwise = before ++ (box:after)
                             where (before, (box:after)) = splitAt index boxes

performOperations :: [Operation] -> [[Lens]] -> [[Lens]]
performOperations [] boxes = boxes
performOperations ((Add label focal):ops) boxes = performOperations ops $ insertLens (label, focal) boxes (hash 0 label)
performOperations ((Remove label):ops) boxes = performOperations ops $ removeLens label boxes (hash 0 label)

calculateFocusingPower :: [[Lens]] -> Int -> [[Int]]
calculateFocusingPower [] _ = []
calculateFocusingPower (box:boxes) index = map (\(slot, (_, focal)) -> index * slot * focal) (zip [1..length box] box) : calculateFocusingPower boxes (index+1)

main :: IO()
main = do
        file <- readFile "Day 15/15.txt"
        let sequence = splitOn file ','
            hashes = map (hash 0) sequence
            operations = createOperations sequence
            boxes = performOperations operations [[] | _ <- [0..255]]
            focusingPower = sum $ map sum $ calculateFocusingPower boxes 1
        putStrLn $ show $ sum hashes
        putStrLn $ show focusingPower