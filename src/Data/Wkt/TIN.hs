module Data.Wkt.TIN (module Data.Wkt.TIN) where

import Data.Wkt.Classes
import Data.Wkt.Triangle
import Data.List (intercalate, group, sort)
import Data.Wkt.Point
import Data.Wkt.Helpers (generateZMString)

newtype TIN a = TIN [Triangle a]

instance Show a => Show (TIN a) where
    show (TIN triangles) = intercalate ", " triangles'
        where
            triangles' = map (\(Triangle triangle) -> "(" <> intercalate ", " (show <$> triangle) <> ")") triangles

instance Show a => ToWKT (TIN a) where
    toWKT (TIN triangles) = "TIN" <> zmString <> "(" <> show triangles <> ")"
        where
            (Triangle firstTriangle) = head triangles
            first = head firstTriangle
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Ord a => Valid (TIN a) where
    isValid (TIN triangles) = validTriangles && isContinuous
        where
            validTriangles = all isValid triangles
            allPoints = concatMap (\(Triangle points) -> points) triangles
            isContinuous =  notElem 1 $ length <$> group (sort allPoints) -- checks if every point is on at least two sides.