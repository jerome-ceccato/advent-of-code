import Text.ParserCombinators.Parsec
import Data.Map (Map, unionWith, elems, fromListWith, assocs, (!))
import qualified Data.Map as Map
import Data.List ( find, sort, maximumBy )
import Data.Bifunctor (second)


printSolution :: (Show a1, Show a2) => a1 -> a2 -> IO ()
printSolution p1 p2 = putStrLn $ "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2

data Action = Sleep | WakeUp deriving (Enum, Show)

data Log = Log {
    minute :: Int,
    entry :: Either Int Action
} deriving (Show)

logParser :: Parser Log
logParser = do
    char '['
    intParser
    char '-'
    intParser
    char '-'
    intParser
    space
    intParser
    char ':'
    minutes <- intParser
    char ']'
    space
    Log minutes <$> entryParser


entryParser :: Parser (Either Int Action)
entryParser = (Left <$> guardParser) <|> (Right <$> actionParser)

guardParser :: Parser Int
guardParser = do
    string "Guard #"
    n <- intParser
    string " begins shift"
    return n

actionParser :: Parser Action
actionParser = do
    (Sleep <$ string "falls asleep") <|> (WakeUp <$ string "wakes up")

intParser :: Parser Int
intParser = read <$> many1 digit

parseLog :: String -> Log
parseLog s = case parse logParser "" s of
    Right e -> e
    Left x -> error . show $ x


-- Solution

-- [(guardId, minute)]
sleptMinutes :: [Log] -> [(Int, Int)]
sleptMinutes logs = loop logs 0 0
    where
    loop [] _ _ = []
    loop (x:xs) guardId sleepM = case entry x of
        Left identifier -> loop xs identifier 0
        Right Sleep -> loop xs guardId (minute x)
        Right WakeUp -> map (guardId,) [sleepM..minute x - 1] ++ loop xs guardId 0

compileGuards :: [(Int, Int)] -> Map Int (Map Int Int)
compileGuards = fromListWith (unionWith (+)) . map (\(g,m) -> (g, Map.singleton m 1))

mostAsleep :: Map Int (Map Int Int) -> Int
mostAsleep = fst . foldl1 (\a b -> if snd a > snd b then a else b) . map (second sum) . assocs

mostAsleepMinute :: Map Int Int -> (Int, Int)
mostAsleepMinute = foldl1 (\a b -> if snd a > snd b then a else b) . assocs

selectId :: (Map Int (Map Int Int) -> Int) -> Map Int (Map Int Int) -> Int
selectId f guards = gid * minute
    where
        gid = f guards
        minute = fst . mostAsleepMinute $ (guards ! gid)

mostVulnerable :: Map Int (Map Int Int) -> Int
mostVulnerable = fst . foldl1 (\a b -> if (snd . snd) a > (snd . snd) b then a else b) . map (second mostAsleepMinute) . assocs

--
main :: IO ()
main = do
    input <- map parseLog . sort . lines <$> readFile "input"
    printSolution 
        (selectId mostAsleep . compileGuards . sleptMinutes $ input) 
        (selectId mostVulnerable . compileGuards . sleptMinutes $ input)
