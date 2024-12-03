{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Data.Char (isDigit)
import Control.Monad(guard)
import Data.Maybe(mapMaybe)
import Control.Applicative((<|>))

readInput :: IO Text
readInput = T.IO.readFile "input/day3.txt"

-- Given a piece of text that starts with "mul(", turn it into a pair of ints if possible
getMul :: Text -> Maybe (Int, Int)
getMul t = do
    (left, right) <- T.break (==',') . T.takeWhile (not . (==')')) <$> T.stripPrefix "mul(" t
    right' <- T.stripPrefix "," right
    guard $ T.all isDigit left && T.all isDigit right'
    guard $  T.length left >= 1 && T.length left <= 3 
          && T.length right' >= 1 && T.length right' <= 3
    pure $ (read (T.unpack left), read (T.unpack right'))

part1 :: Text -> Int
part1 = sum . map (uncurry (*)) . mapMaybe (getMul . snd) . T.breakOnAll "mul("

data Command = 
    Mul Int Int
    | Do
    | Dont
    deriving Show 

getDo :: Text -> Maybe Command
getDo t = Do <$ T.stripPrefix "do()" t

getDont :: Text -> Maybe Command
getDont t = Dont <$ T.stripPrefix "don't()" t

getMul' :: Text -> Maybe Command
getMul' t = uncurry Mul <$> getMul t

getCommands :: Text -> [Command]
getCommands t = mapMaybe (\x -> getDo x <|> getDont x <|> getMul' x) $ T.tails t

calculate :: [Command] -> Int
calculate = go True
  where
    go :: Bool -> [Command] -> Int
    go _ [] = 0
    go _ (Do:cmds) = go True cmds
    go _ (Dont:cmds) = go False cmds
    go False ((Mul _ _):cmds) = go False cmds
    go True ((Mul x y):cmds) = x * y + go True cmds

part2 :: Text -> Int
part2 = calculate . getCommands

main :: IO ()
main = do
    input <- readInput
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)