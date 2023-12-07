import Data.List

-- 1

type Bid = (String, Int)
type Hand = String
type Card = Char

countUniqueCards :: Hand -> [Card] -> [Card]
countUniqueCards [] seen = seen
countUniqueCards (card:hand) seen | any (==card) seen = countUniqueCards hand seen
                                  | otherwise = countUniqueCards hand (card:seen)

countCardOccurence :: Hand -> Card -> Int
countCardOccurence hand card' = length $ filter (==card') hand

handType :: Hand -> Int
handType hand | length uniqueCards == 1 = 6
              | any (==4) $ map (\i -> countCardOccurence hand (uniqueCards !! i)) [0, 1] = 5
              | countCardOccurence hand (uniqueCards !! 0) + countCardOccurence hand (uniqueCards !! 1) == 5 = 4
              | any (==3) $ map (\i -> countCardOccurence hand (uniqueCards !! i)) [0 ,1, 2] = 3
              | length (filter (==2) $ map (\i -> countCardOccurence hand (uniqueCards !! i)) [0, 1, 2]) == 2 = 2
              | any (==2) $ map (\i -> countCardOccurence hand (uniqueCards !! i)) [0 ,1, 2, 3] = 1
              | otherwise = 0
              where uniqueCards = countUniqueCards hand []

cardRanking :: [Card]
cardRanking = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']

cardRank :: Card -> [Card] -> Int
cardRank card (card':ranking) | card == card' = length ranking
                              | otherwise = cardRank card ranking


compareEqual :: Hand -> Hand -> Ordering
compareEqual [] [] = EQ
compareEqual (card1:hand1) (card2:hand2) | ranking1 == ranking2 = compareEqual hand1 hand2
                                         | ranking1 > ranking2 = LT
                                         | ranking1 < ranking2 = GT
                                         where ranking1 = cardRank card1 cardRanking
                                               ranking2 = cardRank card2 cardRanking

compareBids :: Bid -> Bid -> Ordering
compareBids (hand1, _) (hand2, _) | type1 > type2 = LT
                                  | type1 < type2 = GT
                                  | otherwise = compareEqual hand1 hand2
                                  where type1 = handType hand1
                                        type2 = handType hand2

calculateWinnings :: [Bid] -> [Int]
calculateWinnings [] = []
calculateWinnings ((_, bid):bids) = bid * (length bids + 1) : calculateWinnings bids

-- 2

countUniqueNonJokers :: Hand -> [Card] -> [Card]
countUniqueNonJokers [] seen = seen
countUniqueNonJokers (card:hand) seen | any (==card) seen || card == 'J' = countUniqueNonJokers hand seen
                                      | otherwise = countUniqueNonJokers hand (card:seen)

jokerCountCardOccurence :: Hand -> Card -> Int
jokerCountCardOccurence hand card' = length $ filter (\card -> card==card' || card=='J') hand

containsTwoJokerPairs :: Hand -> [Card] -> Bool
containsTwoJokerPairs hand uniqueNonJokers | length (filter (==2) $ map (\i -> countCardOccurence hand (uniqueNonJokers !! i)) [0, 1, 2]) == 2 = True
                                           | jokers > 0 && (any (==2) $ map (\i -> countCardOccurence hand (uniqueNonJokers !! i)) [0 ,1, 2]) = True
                                           | jokers > 1 = True
                                           | otherwise = False
                                           where jokers = countCardOccurence hand 'J'

jokerHandType :: Hand -> Int
jokerHandType hand | length uniqueNonJokers <= 1 = 6
                   | any (==4) $ map (\i -> jokerCountCardOccurence hand (uniqueNonJokers !! i)) [0, 1] = 5
                   | countCardOccurence hand (uniqueNonJokers !! 0) + countCardOccurence hand (uniqueNonJokers !! 1) + jokers == 5 = 4
                   | any (==3) $ map (\i -> jokerCountCardOccurence hand (uniqueNonJokers !! i)) [0 ,1, 2] = 3
                   | containsTwoJokerPairs hand uniqueNonJokers = 2
                   | any (==2) $ map (\i -> jokerCountCardOccurence hand (uniqueNonJokers !! i)) [0 ,1, 2, 3] = 1
                   | otherwise = 0
                    where uniqueNonJokers = countUniqueNonJokers hand []
                          jokers = jokerCountCardOccurence hand 'J'

jokerCardRanking :: [Card]
jokerCardRanking = ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']

compareJokerEqual :: Hand -> Hand -> Ordering
compareJokerEqual [] [] = EQ
compareJokerEqual (card1:hand1) (card2:hand2) | ranking1 == ranking2 = compareJokerEqual hand1 hand2
                                              | ranking1 > ranking2 = LT
                                              | ranking1 < ranking2 = GT
                                               where ranking1 = cardRank card1 jokerCardRanking
                                                     ranking2 = cardRank card2 jokerCardRanking

compareJokerBids :: Bid -> Bid -> Ordering
compareJokerBids (hand1, _) (hand2, _) | type1 > type2 = LT
                                       | type1 < type2 = GT
                                       | otherwise = compareJokerEqual hand1 hand2
                                        where type1 = jokerHandType hand1
                                              type2 = jokerHandType hand2

main :: IO()
main = do
        file <- readFile "Day 7/7.txt"
        let ls = lines file
            bids = map ((\[hand, bid] -> (hand, read bid :: Int)) . words) ls
            sortedBids = sortBy compareBids bids
            sortedJokerBids = sortBy compareJokerBids bids
        putStrLn $ show $ sum $ calculateWinnings sortedBids
        putStrLn $ show $ sum $ calculateWinnings sortedJokerBids