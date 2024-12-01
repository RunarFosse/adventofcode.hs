
-- 1

type Node = (String, (String, String))

parseNodes :: [String] -> [Node]
parseNodes [] = []
parseNodes ("":lines) = parseNodes lines
parseNodes (line:lines) = (current, (left, right)) : parseNodes lines
                        where tokens = words line
                              current = head tokens
                              left = ((drop 1) . reverse . (drop 1) . reverse) $ tokens !! 2
                              right = (reverse . (drop 1) . reverse) $ tokens !! 3

parseMap :: [String] -> (String, [Node])
parseMap lines = (head lines, nodes)
                where nodes = parseNodes $ tail lines

findNode :: [Node] -> String -> Node
findNode (node:nodes) name | fst node == name = node
                           | otherwise = findNode nodes name

traverseNode :: Char -> Node -> [Node] -> Node
traverseNode direction (current, (left, right)) nodes = case direction of
                                        'L' -> findNode nodes left
                                        'R' -> findNode nodes right

traversePath :: String -> Node -> (Node -> Bool) -> (String, [Node]) -> Int
traversePath "" node predicate (path, nodes) = traversePath path node predicate (path, nodes)
traversePath (direction:directions) node@(current, _) predicate (path, nodes) | predicate node = 0
                                                                              | otherwise = 1 + traversePath directions (traverseNode direction node nodes) predicate (path, nodes)

-- 2

findIntersectionTime :: [Int] -> Int
findIntersectionTime [period1, period2] = lcm period1 period2
findIntersectionTime (period:periods) = findIntersectionTime [period, findIntersectionTime periods]

main :: IO()
main = do
        file <- readFile "Day 8/8.txt"
        let (path, nodes) = parseMap $ lines file
            totalSteps = traversePath path (findNode nodes "AAA") (\n -> fst n == "ZZZ") (path, nodes)
            startNodes = (filter (\n -> (head . reverse . fst) n == 'A') nodes)
            totalStepsPerNode = map (\n -> traversePath path n (\n -> (head . reverse . fst) n == 'Z') (path, nodes)) startNodes
        putStrLn $ show totalSteps
        putStrLn $ show $ findIntersectionTime totalStepsPerNode