import Data.List
import Data.Array
import GaussElimination

-- 1

splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn c' (c:cs) | c' == c = "" : splitOn c' cs
                  | otherwise = (c:current) : rest
                  where (current:rest) = splitOn c' cs

type Position = (Double, Double, Double)
type Velocity = (Double, Double, Double)
type Hail = (Position, Velocity)

extractHail :: String -> Hail
extractHail line = ((read px, read py, read pz), (read vx, read vy, read vz))
                 where [position, velocity] = (map words . splitOn '@') line
                       [px, py, pz] = map init (init position) ++ [last position]
                       [vx, vy, vz] = map init (init velocity) ++ [last position]

blowHail :: Hail -> Double -> Hail
blowHail ((px, py, pz), (vx, vy, vz)) time = ((px + vx*time, py + vy*time, pz + vz*time), (vx, vy, vz))

areXYParallel :: Hail -> Hail -> Bool
areXYParallel (_, (vx1, vy1, _)) (_, (vx2, vy2, _)) = vx1 * vy2 - vx2 * vy1 == 0

isBounded :: Hail -> (Double, Double) -> Bool
isBounded ((px, py, _), _) (start, end) = (px >= start && px <= end) && (py >= start && py <= end)

hasFutureXYCollision :: Hail -> Hail -> (Double, Double) -> Bool
hasFutureXYCollision hail1 hail2 bounds | areXYParallel hail1 hail2 = False
                                        | otherwise = (t1 >= 0 && t2 >= 0) && (isBounded (blowHail hail1 t1) bounds && isBounded (blowHail hail2 t2) bounds)
                                        where ((px1, py1, _), (vx1, vy1, _)) = hail1
                                              ((px2, py2, _), (vx2, vy2, _)) = hail2
                                              [t1, t2] = map fromRational $ gaussEliminationFromEquation [[toRational (-vx1), toRational vx2], [toRational (-vy1), toRational vy2]] [[toRational $ px1-px2], [toRational $ py1-py2]]

pairs :: [Hail] -> [(Hail, Hail)]
pairs hail = [(hail1, hail2) | (hail1:rest) <- tails hail, hail2 <- rest]

-- 2 Very hard in haskell, done in Python:

{-

from sympy import symbols, solve

hail = [tuple(map(int, line.replace("@", ",").split(","))) for line in open("Day 24/24.txt")]

px, py, pz, vx, vy, vz = symbols("px, py, pz, vx, vy, vz")

equations = []

for i, (pxi, pyi, pzi, vxi, vyi, vzi) in enumerate(hail):
    equations.append((px - pxi) * (vy - vyi) - (py - pyi) * (vx - vxi))
    equations.append((px - pxi) * (vz - vzi) - (pz - pzi) * (vx - vxi))

    if i > 2:
        valid_solutions = [solution for solution in solve(equations) if all(x % 1 == 0 for x in solution.values())]
        break

position_sum = valid_solutions[0][px] + valid_solutions[0][py] + valid_solutions[0][pz]
print(position_sum)

-}

main :: IO()
main = do
        file <- readFile "Day 24/24.txt"
        let hail = map extractHail (lines file)
            collisionBounds = (200000000000000, 400000000000000)
            collisions = (length . filter (==True) . map (\(hail1, hail2) -> hasFutureXYCollision hail1 hail2 collisionBounds)) $ pairs hail
        putStrLn $ show collisions
        putStrLn "Check comment under '-- 2' for Python solution."