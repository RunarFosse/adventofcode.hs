import Data.Char
import Data.List

-- 1

type DiskMap = [(Int, Int)]

parseDiskMap :: String -> DiskMap
parseDiskMap = (zip ((concat . transpose) [[0..], repeat (-1)]) . map digitToInt) 

createCompactDisk :: DiskMap -> DiskMap -> [Int]
createCompactDisk [] _ = []
createCompactDisk diskMap ((-1, _):diskMapReverse) = createCompactDisk diskMap diskMapReverse
createCompactDisk ((-1, blocks):diskMap) ((idLast, blocksLast):diskMapReverse) | blocks < blocksLast = replicate blocks idLast ++ createCompactDisk diskMap ((idLast, blocksLast - blocks):diskMapReverse)
                                                                               | blocks > blocksLast = replicate blocksLast idLast ++ createCompactDisk ((-1, blocks - blocksLast):diskMap) diskMapReverse
                                                                               | otherwise = replicate blocksLast idLast ++ createCompactDisk diskMap diskMapReverse
createCompactDisk ((id, blocks):diskMap) ((idLast, blocksLast):diskMapReverse) | id == idLast = replicate (min blocks blocksLast) id
                                                                               | otherwise = replicate blocks id ++ createCompactDisk diskMap ((idLast, blocksLast):diskMapReverse)
                                         

computeChecksum :: [Int] -> Int
computeChecksum = (sum . map (\(i, id) -> i * id) . zip [0..])

-- 2

createWholeFileCompactDiskMap :: DiskMap -> DiskMap -> DiskMap
createWholeFileCompactDiskMap diskMap [] = diskMap
createWholeFileCompactDiskMap diskMap ((-1, _):diskMapReverse) = createWholeFileCompactDiskMap diskMap diskMapReverse
createWholeFileCompactDiskMap diskMap ((idLast, blocksLast):diskMapReverse) = case findIndex (\(id, blocks) -> blocksLast <= blocks && id == -1) diskMap of
                                                                                Just i -> if i > j 
                                                                                      then createWholeFileCompactDiskMap diskMap diskMapReverse
                                                                                      else createWholeFileCompactDiskMap (spliceInto i (idLast, blocksLast) diskMap) diskMapReverse
                                                                                Nothing -> createWholeFileCompactDiskMap diskMap diskMapReverse
                                                                            where Just j = findIndex (\(idOther, _) -> idOther == idLast) diskMap
                                                                                  spliceInto :: Int -> (Int, Int) -> DiskMap -> DiskMap
                                                                                  spliceInto i (idLast, blocksLast) diskMap | blocksRemoved == blocksLast = take i diskMap ++ [(idLast, blocksLast)] ++ drop (i+1) (take j diskMap ++ [(-1, blocksLast)] ++ drop (j+1) diskMap)
                                                                                                                            | otherwise = take i diskMap ++ [(idLast, blocksLast), (-1, blocksRemoved - blocksLast)] ++ drop (i+1) (take j diskMap ++ [(-1, blocksLast)] ++ drop (j+1) diskMap)
                                                                                    where (_, blocksRemoved) = diskMap !! i

diskMapToCompactDisk :: DiskMap -> [Int]
diskMapToCompactDisk = foldr (\(id, blocks) r -> replicate blocks (if id == -1 then 0 else id) ++ r) []

main :: IO()
main = do
        file <- readFile "Day 9/9.txt"
        let diskMap = parseDiskMap file
            compactDisk = createCompactDisk diskMap (reverse diskMap)
            wholeFileCompactDisk = diskMapToCompactDisk $ createWholeFileCompactDiskMap diskMap (reverse diskMap)
        putStrLn $ show $ computeChecksum compactDisk
        putStrLn $ show $ computeChecksum wholeFileCompactDisk
