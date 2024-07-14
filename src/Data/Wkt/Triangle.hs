module Data.Wkt.Triangle (module Data.Wkt.Triangle) where
    
import Data.Wkt.Classes
import Data.Wkt.Point

import Data.List (intercalate)
import Data.Maybe (isJust)

newtype Triangle a  = Triangle [Point a]

instance Show a => Show (Triangle a) where
    show (Triangle vertices) = intercalate ", " (show <$> vertices)

instance Show a => ToWKT (Triangle a) where
    toWKT triangle = "Triangle" <> zmString <> "(" <> show triangle <> ")"
        where
            Triangle vertices = triangle
            first = head vertices
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "

instance Eq a => Valid (Triangle a) where
    isValid (Triangle lines') = firstPoint == lastPoint && size == 4
        where
            firstPoint = head  lines'
            lastPoint = head lines'
            size = length lines'