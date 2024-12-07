-- 1

type Equation = (Int, [Int])

readEquations :: String -> [Equation]
readEquations file = (map (wordsToEquation . words) . lines) file
                  where wordsToEquation :: [String] -> Equation
                        wordsToEquation words = (read (init $ head words) :: Int, map (\number -> read number :: Int) $ tail words)

canBeTrue :: Equation -> Bool
canBeTrue equation = canBeTrueHelper equation 0
                  where canBeTrueHelper :: Equation -> Int -> Bool
                        canBeTrueHelper (result, []) cumulation = result == cumulation
                        canBeTrueHelper (result, n:ns) cumulation | cumulation > result = False
                                                                  | otherwise = canBeTrueHelper (result, ns) (cumulation + n) || canBeTrueHelper (result, ns) (cumulation * n)
                        

-- 2

newCanBeTrue :: Equation -> Bool
newCanBeTrue equation = newCanBeTrueHelper equation 0
                  where newCanBeTrueHelper :: Equation -> Int -> Bool
                        newCanBeTrueHelper (result, []) cumulation = result == cumulation
                        newCanBeTrueHelper (result, n:ns) cumulation | cumulation > result = False
                                                                     | otherwise = newCanBeTrueHelper (result, ns) (read (show cumulation ++ show n) :: Int) || newCanBeTrueHelper (result, ns) (cumulation + n) || newCanBeTrueHelper (result, ns) (cumulation * n)

main :: IO()
main = do
        file <- readFile "Day 7/7.txt"
        let equations = readEquations file
            calibrations = filter canBeTrue equations
            newCalibrations = filter newCanBeTrue equations
        putStrLn $ show $ (sum . map fst) calibrations
        putStrLn $ show $ (sum . map fst) newCalibrations