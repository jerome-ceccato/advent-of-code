import Text.ParserCombinators.Parsec
import Data.Map (Map, fromList, unionWith, elems)
import qualified Data.Map as Map


printSolution :: (Show a1, Show a2) => a1 -> a2 -> IO ()
printSolution p1 p2 = putStrLn $ "Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2

data Point = Point { x :: Int, y :: Int }
    deriving (Show, Ord, Eq)

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

fabric :: Entry -> [Point]
fabric entry = [Point (posx entry + x) (posy entry + y) | x <- [0..width entry - 1], y <- [0..height entry - 1]]

countValid :: Map Point Int -> Int
countValid = length . filter (> 1) . elems

accumulate :: [Entry] -> Map Point Int
accumulate = foldl1 (unionWith (+)) . map (fromList . map (, 1) . fabric)

main :: IO ()
main = do
    input <- parseClaim <$> readFile "input"
    printSolution (countValid . accumulate $ input) "(part2 input)"
