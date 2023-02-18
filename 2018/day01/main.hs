import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

printSolution :: (Show a1, Show a2) => a1 -> a2 -> IO ()
printSolution p1 p2 = putStrLn $ "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2

asIntArray :: String -> [Int]
asIntArray = map (read . dropWhile (== '+')) . words

-- Solution

part2 :: [Int] -> Int
part2 numbers = part2' (cycle numbers) 0 (IntSet.singleton 0)
    where
        part2' [] _ _ = -1
        part2' (x:xs) acc prev = let next = acc + x in
            if IntSet.member next prev then next else part2' xs next (IntSet.insert next prev)


main :: IO ()
main = do
    input <- asIntArray <$> readFile "input"
    printSolution (sum input) (part2 input)
