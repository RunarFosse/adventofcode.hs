-- 1

data Rotation = L Int | R Int deriving Show

extractRotation :: String -> Rotation
extractRotation operation | head operation == 'L' = L turns
                          | otherwise = R turns
                          where turns = read (tail operation) :: Int

rotatePosition :: Rotation -> Int -> Int
rotatePosition rotation position = case rotation of 
                                    L turns -> mod (position - turns) 100
                                    R turns -> mod (position + turns) 100

computePassword :: [String] -> Int -> Int
computePassword [] _ = 0
computePassword operations position = increment + computePassword (tail operations) newPosition
                                    where rotation = (extractRotation . head) operations
                                          newPosition = rotatePosition rotation position
                                          increment = if newPosition == 0 then 1 else 0

-- 2

countZeroClicks :: Rotation -> Int -> Int
countZeroClicks rotation position = case rotation of
                                        L turns -> div (mod (100 - position) 100 + turns) 100
                                        R turns -> div (position + turns) 100

computeSecurePassword :: [String] -> Int -> Int
computeSecurePassword [] _ = 0
computeSecurePassword operations position = increment + computeSecurePassword (tail operations) newPosition
                                    where rotation = (extractRotation . head) operations
                                          newPosition = rotatePosition rotation position
                                          increment = countZeroClicks rotation position

main :: IO()
main = do
        file <- readFile "Day 1/1.txt"
        let operations = lines file
            password = computePassword operations 50
            securePassword = computeSecurePassword operations 50
        putStrLn $ show $ password
        putStrLn $ show $ securePassword
        