{-# LANGUAGE OverloadedStrings#-}
module Data.Wkt.MultiPolygon (module Data.Wkt.MultiPolygon) where

import Data.Wkt.Classes
import Data.Wkt.Polygon
import Data.Wkt.LineString
import Data.Wkt.Point
import Data.Wkt.Helpers
import Data.List (intercalate)
import Data.Text (pack)

newtype MultiPolygon a = MultiPolygon [Polygon a]

instance Show a => Show (MultiPolygon a) where
    show (MultiPolygon polygons) = intercalate ", " polygons'
        where
            polygons' =showP <$> polygons

instance Show a => ToWKT (MultiPolygon a) where
    toWKT multiPolygon = "MultiPolygon" <> zmString <> "(" <> pack (show multiPolygon) <> ")"
        where
            (MultiPolygon polygons) = multiPolygon
            (Polygon firstPolygon) = head polygons
            (LineString firstLine) = head firstPolygon
            first = head firstLine
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Eq a => Valid (MultiPolygon a) where
    isValid (MultiPolygon polygons') = all isValid polygons'