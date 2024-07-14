module Data.Wkt.Primitives (module Data.Wkt.Primitives) where

import Data.Wkt.Classes
import Data.Wkt.Point
import Data.Wkt.LineString
import Data.Wkt.Polygon
import Data.Wkt.Triangle

data Primitives a = PrimPoint (Point a)| PrimLine (LineString a)| PrimPolygon (Polygon a)| PrimTriangle (Triangle a)

instance Show a => Show (Primitives a) where
    show (PrimPoint a)    = show a
    show (PrimLine a)     = show a
    show (PrimPolygon a)  = show a
    show (PrimTriangle a) = show a

instance Show a => ToWKT (Primitives a) where
    toWKT (PrimPoint a)    = toWKT a
    toWKT (PrimLine a)     = toWKT a
    toWKT (PrimPolygon a)  = toWKT a
    toWKT (PrimTriangle a) = toWKT a

instance Eq a => Valid (Primitives a) where
    isValid (PrimPoint a)    = isValid a
    isValid (PrimLine a)     = isValid a
    isValid (PrimPolygon a)  = isValid a
    isValid (PrimTriangle a) = isValid a