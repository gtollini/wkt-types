module Data.Wkt.Polygon (module Data.Wkt.Polygon) where

import Data.Wkt.Classes
import Data.Wkt.Point
import Data.Wkt.LineString

import Data.List (intercalate)
import Data.Wkt.Helpers (generateZMString)

newtype Polygon a = Polygon [LineString a]

instance Show a => Show (Polygon a) where
    show (Polygon polygon) = intercalate ", " rings
        where
            rings = map (\(LineString ring) -> "(" <> intercalate ", " (show <$> ring) <> ")") polygon

instance Show a => ToWKT (Polygon a) where
    toWKT polygon = "Polygon" <> zmString <> "(" <> show polygon <> ")"
        where
            Polygon rings = polygon
            (LineString firstLine) = head rings
            first = head firstLine
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Eq a => Valid (Polygon a) where
    isValid (Polygon lines') = validLines && validPolygon
        where
            validLines = all isValid lines'
            validPolygon = all (\(LineString line) -> head line == last line) lines'