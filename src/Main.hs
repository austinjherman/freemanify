import System.Environment (getArgs)
import Data.Char (toUpper, toLower)


mapToN :: Int -> Int -> (a -> a) -> [a] -> [a]
mapToN n o f = zipWith ($) (setOff o . drop 1 . cycle . take n $ f : repeat id)
  where setOff o | o < 0 = drop (abs o)
                 | otherwise = ((replicate o id) ++)

mapToOdd :: (a -> a) -> [a] -> [a]
mapToOdd = mapToN 2 (-1)

mapToEven :: (a -> a) -> [a] -> [a]
mapToEven = mapToN 2 0

freemanify :: String -> String
freemanify = (mapToOdd toLower) . (mapToEven toUpper)

getFlags :: [String] -> [String]
getFlags strs = [x | x <- strs, take 2 x == "--"]

stripFlags :: [String] -> [String]
stripFlags strs = [ x | x <- strs, take 2 x /= "--"]

emoify :: String -> String
emoify str = "xXx " ++ str ++ " xXx"


main = do
  input <- getArgs
  case (elem "--emo" . getFlags) input of
    True -> mapM (putStrLn . emoify . freemanify) (stripFlags input)
    False -> mapM (putStrLn . freemanify) (stripFlags input)
