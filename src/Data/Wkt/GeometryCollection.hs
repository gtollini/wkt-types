module Data.Wkt.GeometryCollection (module Data.Wkt.GeometryCollection) where

import Data.Wkt.Classes
import Data.Wkt.Primitives
import Data.Wkt.LineString
import Data.Wkt.Triangle
import Data.Wkt.Polygon
import Data.Wkt.Point
import Data.Wkt.Helpers (generateZMString)
import Data.List (intercalate)

newtype GeometryCollection a =  GeometryCollection [Primitives a]

instance Show a => Show (GeometryCollection a) where
    show (GeometryCollection collection) = intercalate ", " (show <$> collection)

instance Show a => ToWKT (GeometryCollection a) where
    toWKT (GeometryCollection collection) = "GeometryCollection" <> zmString <> "( " <> intercalate ", " (toWKT <$> collection) <> ")"
        where
            first = case head collection of
                PrimPoint a                 -> a
                PrimLine (LineString a)     -> head a
                PrimTriangle (Triangle a)   -> head a
                PrimPolygon (Polygon a)     -> (\(LineString a') -> head a') $ head a
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Eq a => Valid (GeometryCollection a) where
    isValid (GeometryCollection collection') = all isValid collection'