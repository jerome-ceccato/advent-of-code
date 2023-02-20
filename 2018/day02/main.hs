import Data.List ( group, sort, tails, find )
import Data.Maybe ( fromJust )

printSolution :: (Show a1, Show a2) => a1 -> a2 -> IO ()
printSolution p1 p2 = putStrLn $ "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2

-- Solution

hasCount :: Int -> String -> Bool
hasCount n id = n `elem` map length (group . sort $ id)

containsMultiples :: [String] -> Int -> Int
containsMultiples ids n = length . filter (hasCount n) $ ids

part1 :: [String] -> Int
part1 input = containsMultiples input 2 * containsMultiples input 3

differenceScore :: String -> String -> Int
differenceScore a b = length . filter (uncurry (/=)) $ zip a b

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

strictIntersection :: Eq a => [a] -> [a] -> [a]
strictIntersection [] _ = []
strictIntersection _ [] = []
strictIntersection (a:as) (b:bs)
    | a == b = a : strictIntersection as bs
    | otherwise = strictIntersection as bs

part2 :: [String] -> String
part2 input = uncurry strictIntersection . fromJust . find (\(a,b) -> differenceScore a b == 1) $ pairs input


main :: IO ()
main = do
    input <- words <$> readFile "input"
    printSolution (part1 input) (part2 input)
