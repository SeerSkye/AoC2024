module Main where
import qualified Data.Map.Strict as M

readInput :: IO (M.Map (Int, Int) Char)
readInput = M.fromList . concat . zipWith (\y l -> zipWith (\x c -> ((x, y), c)) [0..] l) [0..] . lines <$> readFile "input/day4.txt"

xmasShapes :: [[(Char, (Int, Int))]]
xmasShapes = let
    fdirs = [(0,1), (1, 0), (1, 1), (1, -1)] 
    allDirs = fdirs ++ map (\(x, y) -> (-x, -y)) fdirs
    shapes = map (\(x, y) -> map (\n -> (n * x, n * y)) [0..3]) allDirs
  in
    map (zip "XMAS") shapes

-- check if a given xmas shape appears at a given coordinate in the grid
checkXmas :: M.Map (Int, Int) Char -> (Int, Int) -> [(Char, (Int, Int))] -> Bool
checkXmas m (x, y) pat = 
    all (\(c, (offsetX, offsetY)) -> M.findWithDefault '.' (x + offsetX, y + offsetY) m == c) pat

part1 :: M.Map (Int, Int) Char -> Int
part1 m = sum $ M.mapWithKey (\k _ -> length $ filter id $ map (checkXmas m k) xmasShapes) m

masShapes :: [[(Char, (Int, Int))]]
masShapes = let 
    baseMas = [('A', (0, 0)), ('M', (-1, -1)), ('M', (-1, 1)), ('S', (1, -1)), ('S', (1, 1))]
    rot180Mas = baseMas:[map (\(c, (x, y)) -> (c, (-x, -y))) baseMas] 
    allMas = rot180Mas ++ (map . map) (\(c, (x, y)) -> (c, (y, -x))) rot180Mas
  in
    allMas

part2 :: M.Map (Int, Int) Char -> Int
part2 m = sum $ M.mapWithKey (\k _ -> length $ filter id $ map (checkXmas m k) masShapes) m

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)