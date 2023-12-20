import qualified Data.Map as M

-- 1

data Pulse = High | Low deriving (Show, Eq)
data Module = Broadcaster [String] | FlipFlop Bool [String] | Conjunction Memory [String] deriving (Show, Eq)

type Memory = M.Map String Pulse
type Modules = M.Map String Module

addConnection :: Modules -> String -> String -> Modules
addConnection modules from name = case M.lookup name modules of
                                        Just (Conjunction memory connected) -> M.insert name (Conjunction (M.insert from Low memory) connected) modules
                                        _ -> modules

addConnections :: Modules -> String -> [String] -> Modules
addConnections modules _ [] = modules
addConnections modules from (name:connected) = addConnections (addConnection modules from name) from connected

initializeConjunctions :: Modules -> [(String, [String])] -> Modules
initializeConjunctions modules [] = modules
initializeConjunctions modules ((name, connected):rest) = initializeConjunctions connectedModules rest
                                                        where connectedModules = addConnections modules (if head name == 'b' then name else drop 1 name) connected

createModules :: [String] -> Modules
createModules lines = initializeConjunctions modules adjs
                    where modules = M.fromList $ map createModule adjs
                          adjs = map (\line -> let tokens = words line in (head tokens, map init ((init . drop 2) tokens) ++ [last tokens])) lines
                          createModule :: (String, [String]) -> (String, Module)
                          createModule ("broadcaster", connected) = ("broadcaster", Broadcaster connected)
                          createModule ('%':name, connected) = (name, FlipFlop False connected)
                          createModule ('&':name, connected) = (name, Conjunction M.empty connected)

orderPulse :: Modules -> String -> [String] -> Pulse -> [((String, String), Module, Pulse)]
orderPulse modules from names pulse = map (\name -> ((from, name), modules M.! name, pulse)) $ filter (\name -> M.member name modules) names

sendPulse :: Modules -> [((String, String), Module, Pulse)] -> (Int, Int) -> (Modules, (Int, Int))
sendPulse modules [] pulses = (modules, pulses)
sendPulse modules ((_, Broadcaster connected, pulse):queue) (low, high) = sendPulse modules (queue ++ (orderPulse modules "broadcaster" connected pulse)) $ if pulse == Low then (low+length connected, high) else (low, high+length connected)
sendPulse modules (((_, name), FlipFlop flipped connected, pulse):queue) (low, high) | pulse == High = sendPulse modules queue (low, high)
                                                                                     | otherwise = sendPulse (M.insert name (FlipFlop (not flipped) connected) modules) (queue ++ (orderPulse modules name connected (if flipped then Low else High))) $ if flipped then (low+length connected, high) else (low, high+length connected)
sendPulse modules (((from, name), Conjunction memory connected, pulse):queue) (low, high) = sendPulse (M.insert name (Conjunction updatedMemory connected) modules) (queue ++ (orderPulse modules name connected (if all (==High) updatedMemory then Low else High))) $ if all (==High) updatedMemory then (low+length connected, high) else (low, high+length connected)
                                                                                          where updatedMemory = M.insert from pulse memory


pushButton :: Modules -> Int -> (Int, Int)
pushButton modules 0 = (0, 0)
pushButton modules count = (lows + fst nextPulses, highs + snd nextPulses)
                         where (pulsedModules, (lows, highs)) = sendPulse modules (orderPulse modules "" ["broadcaster"] Low) (1, 0)
                               nextPulses = pushButton pulsedModules (count-1)

-- 2

getConnected :: Module -> [String]
getConnected module' = case module' of
                        Broadcaster connected -> connected
                        FlipFlop _ connected -> connected
                        Conjunction _ connected -> connected

getMemory :: Module -> Memory
getMemory (Conjunction memory _) = memory
getMemory _ = error "Module is not a conjunction"

type MemoryPeriods = M.Map String Int

updateMemory :: MemoryPeriods -> [(String, Pulse)] -> Int -> MemoryPeriods
updateMemory periods [] _ = periods
updateMemory periods ((name, pulse):memory) time | pulse == High && M.notMember name periods = updateMemory (M.insert name time periods) memory time
                                                 | otherwise = updateMemory periods memory time

calculatePeriodOfConjunctionMemory :: Modules -> String -> Int -> MemoryPeriods -> MemoryPeriods
calculatePeriodOfConjunctionMemory modules conjunction buttonPresses periods | length periods == (length . getMemory) (modules M.! conjunction) = periods
                                                                             | otherwise = calculatePeriodOfConjunctionMemory pulsedModules conjunction (buttonPresses+1) updatedPeriods
                                                                             where (pulsedModules, _) = sendPulse modules (orderPulse modules "" ["broadcaster"] Low) (1, 0)
                                                                                   updatedPeriods = updateMemory periods ((M.assocs . getMemory . (pulsedModules M.!)) conjunction) buttonPresses

calculatePeriodOfConjunctionsMemory :: Modules -> String -> MemoryPeriods
calculatePeriodOfConjunctionsMemory modules conjunctionControllingRx = calculatePeriodOfConjunctionMemory modules conjunctionControllingRx 1 M.empty

main :: IO()
main = do
        file <- readFile "Day 20/20.txt"
        let modules = createModules $ lines file
            pulses = pushButton modules 1000
            conjunctionControllingRx = (head . M.keys . M.filter (\module' -> any (=="rx") $ getConnected module')) modules
            conjunctionsControllingConjunctionControllingRx = (M.keys . M.filter (\module' -> any (==conjunctionControllingRx) $ getConnected module')) modules
            conjunctionsControllingConjunctionControllingConjunctionControllingRx = concat $ map (\indirectlyControllingRx -> (M.keys . M.filter (\module' -> any (==indirectlyControllingRx) $ getConnected module')) modules) conjunctionsControllingConjunctionControllingRx
            conjunctionPeriods = map (sum . M.elems . calculatePeriodOfConjunctionsMemory modules) conjunctionsControllingConjunctionControllingConjunctionControllingRx
            totalButtonPressesForSandMachineToReceiveLowPulse = foldl lcm 1 conjunctionPeriods
        putStrLn $ show (fst pulses * snd pulses)
        putStrLn $ show totalButtonPressesForSandMachineToReceiveLowPulse