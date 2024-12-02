module Main where

readInput :: IO [[Int]]
readInput = fmap (fmap read . words) . lines <$> readFile "input/day2.txt"

isSafe :: [Int] -> Bool
isSafe l = 
    let differences = zipWith (-) l (drop 1 l)
    in all (\x -> x >= 1 && x <= 3) differences || all (\x -> x <= (-1) && x >= (-3)) differences

part1 :: [[Int]] -> Int
part1 = length . filter isSafe

problemDampener :: [Int] -> Bool
problemDampener l = any isSafe (dampenedLists)
  where 
    len = length l
    dampenedLists :: [[Int]]
    dampenedLists = fmap (\x -> take x l ++ drop (x + 1) l) [0..len]

part2 :: [[Int]] -> Int
part2 = length . filter problemDampener

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)