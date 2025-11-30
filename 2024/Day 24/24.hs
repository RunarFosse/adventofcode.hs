import Data.Bits
import Data.Maybe
import Data.Tuple
import Data.List
import qualified Data.Map as M

-- 1

type System = M.Map String Bool
type Operations = M.Map String (String, String, String)

initializeSystem :: String -> (System, Operations)
initializeSystem file = (M.fromList initialBits, operations)
                      where initialBits = (map (\line -> (take 3 line, last line == '1')) . takeWhile (/="") . lines) file
                            operations = (parseOperations . tail . dropWhile (/="") . lines) file

parseOperations :: [String] -> Operations
parseOperations [] = M.empty
parseOperations (line:lines) = M.insert result (operation, input1, input2) (parseOperations lines)
                            where (input1:operation:input2:_:result:[]) = words line

compute :: System -> Operations -> String -> (Bool, System)
compute system operations variable | M.member variable system = (system M.! variable, system)
                                   | otherwise = (result, M.insert variable result system2)
                                   where (operation, input1, input2) = operations M.! variable
                                         (value1, system1) = compute system operations input1
                                         (value2, system2) = compute system1 operations input2
                                         result = case operation of
                                                    "AND" -> value1 && value2
                                                    "OR" -> value1 || value2
                                                    "XOR" -> (value1 || value2) && not (value1 && value2)

computeBinary :: System -> Operations -> Char -> (Int, System)
computeBinary system operations variable = foldr (\variableBit (total, system) -> let (result, newSystem) = compute system operations variableBit in (if result then setBit total (read (tail variableBit) :: Int) else total, newSystem)) (0, system) variableBits
                                         where variableBits = filter ((==variable) . head) (M.keys system ++ M.keys operations)

-- 2

findOperation :: Operations -> String -> String -> String -> Maybe String
findOperation operations op variable1 variable2 | null results = Nothing
                                                | otherwise = Just (head results)
                                                where matchesGate :: String -> Bool
                                                      matchesGate key = let (operation, input1, input2) = operations M.! key in operation == op && (variable1, variable2) `elem` [(input1, input2), swap (input1, input2)]
                                                      results = filter matchesGate (M.keys operations)

swapOperations :: Operations -> String -> String -> Operations
swapOperations operations result1 result2 = M.insert result1 operation2 (M.insert result2 operation1 operations)
                                          where operation1 = operations M.! result1
                                                operation2 = operations M.! result2

findSwaps :: Operations -> Int -> Int -> (Operations, String)
findSwaps operations bit maxBit | bit == 0 = if currentXOR /= currentZ then findSwaps (swapOperations operations currentXOR currentZ) bit maxBit
                                             else (operations, currentAND)
                                | bit == maxBit = if currentCarry /= currentZ then findSwaps (swapOperations newOperations currentCarry currentZ) bit maxBit
                                                  else (newOperations, "")
                                | otherwise = case currentADDER of
                                                 Just result -> if result /= currentZ then findSwaps (swapOperations newOperations result currentZ) bit maxBit
                                                                else (newOperations, nextCarry)
                                                 Nothing -> findSwaps (swapOperations newOperations currentXOR currentAND) bit maxBit
                        where currentX = (('x':) . drop 1 . show . (+100)) bit
                              currentY = (('y':) . drop 1 . show . (+100)) bit
                              currentZ = (('z':) . drop 1 . show . (+100)) bit
                              currentXOR = fromJust (findOperation operations "XOR" currentX currentY)
                              currentAND = fromJust (findOperation operations "AND" currentX currentY)
                              currentADDER = findOperation operations "XOR" currentXOR currentCarry
                              currentDigitSetter = fromJust (findOperation operations "AND" currentXOR currentCarry)
                              nextCarry = fromJust (findOperation operations "OR" currentAND currentDigitSetter)
                              (newOperations, currentCarry) = findSwaps operations (bit - 1) maxBit
                              

main :: IO ()
main = do
        file <- readFile "Day 24/24.txt"
        let (system, operations) = initializeSystem file
            (z, newSystem) = computeBinary system operations 'z'
            mostSignificantBit = (maximum . map (\variable -> read (tail variable) :: Int) . filter ((=='z') . head) . M.keys) operations
            (newOperations, _) = findSwaps operations mostSignificantBit mostSignificantBit
            swappedOperations = filter (\key -> operations M.! key /= newOperations M.! key) (M.keys newOperations)
        putStrLn $ show z
        putStrLn $ intercalate "," swappedOperations