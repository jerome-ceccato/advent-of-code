import Text.ParserCombinators.Parsec
import Data.Array

printSolution :: (Show a1, Show a2) => a1 -> a2 -> IO ()
printSolution p1 p2 = putStrLn $ "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2

data Point = Point { x :: Int, y :: Int }
    deriving (Show)

data Entry = Entry {
    id :: Int,
    pos :: Point,
    size :: Point
} deriving (Show)

claimParser :: Parser Entry
claimParser = do
    char '#'
    id <- intParser
    space
    char '@'
    space
    pos <- pointParser
    char ':'
    space
    Entry id pos <$> pointParser

pointParser :: Parser Point
pointParser = do
    x <- intParser
    char ',' <|> char 'x'
    Point x <$> intParser

intParser :: Parser Int
intParser = read <$> many1 digit

parseClaim :: String -> [Entry]
parseClaim s = case parse (sepEndBy claimParser newline) "" s of
    Right e -> e
    Left x -> error . show $ x


posx :: Entry -> Int
posx = x . pos

posy :: Entry -> Int
posy = y . pos

width :: Entry -> Int
width = x . size

height :: Entry -> Int
height = y . size

-- Solution

fabricSize :: Int
fabricSize = 1000

entryOverlaps :: Int -> Int -> Entry -> Bool
entryOverlaps x y entry = x > posx entry && x <= (posx entry + width entry) &&
    y > posy entry && y <= (posy entry + height entry)

fabric :: [Entry] -> [[Int]]
fabric entries = [[length . filter (entryOverlaps x y) $ entries | x <- [0..fabricSize]] | y <- [0..fabricSize]]

countValid :: [[Int]] -> Int
countValid = length . filter (> 1) . concat

main :: IO ()
main = do
    input <- parseClaim <$> readFile "input"
    printSolution (countValid . fabric $ input) "(part2 input)"
