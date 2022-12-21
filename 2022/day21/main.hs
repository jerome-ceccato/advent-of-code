import Numeric

printResult :: Double -> IO ()
printResult x = putStrLn $ showFFloat Nothing x ""

main :: IO ()
main = mapM_ printResult [(part1), (part2)]

part1 :: Double
part1 = root where
    -- {{part1}}

part2 :: Double
part2 = findn 0 10000000000000

findn :: Double -> Double -> Double
findn low high = 
    let n = low + (high - low) / 2 in
    case compare (shout n) 0 of
        GT -> findn n high
        LT -> findn low n
        EQ -> n

shout :: Double -> Double
shout humn = (rvrh - hzgl) where
    -- {{part2}}
