module Main where
import Data.List(transpose, sort)

splitLine :: String -> [String]
splitLine txt = [take 5 txt, drop 8 txt]

readInput :: IO [[Int]]
readInput = transpose . map (map read . splitLine) . lines <$> readFile "input/day1.txt"

part1 :: [[Int]] -> Int
part1 = sum . foldr (zipWith (\x y -> abs (x - y))) (repeat 0) . map sort

part2 :: [[Int]] -> Int
part2 [leftList, rightList] = sum $ zipWith (*) leftList $ map (\x -> length $ filter (== x) rightList) leftList
part2 _ = error "I need 2 lists!!"

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)
