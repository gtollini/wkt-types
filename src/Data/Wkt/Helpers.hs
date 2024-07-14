module Data.Wkt.Helpers (module Data.Wkt.Helpers) where
import Data.Maybe (isJust)
import Data.Wkt.Point (Point (..))
import Data.Wkt.Triangle

generateZMString :: Maybe a -> Maybe a -> String
generateZMString z' m'
    |isJust z' && isJust m' = " ZM "
    |isJust z' = " Z "
    |isJust m' = " M "
    |otherwise = " "

pointDimension :: Point a -> Int
pointDimension (Point _ _ z' m')
    | isJust m' = 4
    | isJust z' = 3
    | otherwise = 2

showP :: Show a => a -> String
showP = ("(" <>) . (<> ")") . show

-- Must be valid Triangle
allSides :: Triangle a-> [(Point a, Point a)]
allSides (Triangle vertices) = allPairs vertices

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs [_] = []
allPairs (x1:x2:xs) = (x1,x2) : allPairs (x2:xs)