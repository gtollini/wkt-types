module Data.Wkt.LineString (module Data.Wkt.LineString) where

import Data.Wkt.Classes
import Data.Wkt.Point (Point(..))
import Data.List (intercalate)
import Data.Wkt.Helpers (generateZMString, pointDimension)

newtype LineString a = LineString [Point a]

instance Show a => Show (LineString a) where
    show (LineString line) = intercalate ", " (show <$> line)

instance Show a => ToWKT (LineString a) where
    toWKT lineString
        | null line = "EMPTY"
        | otherwise = "LineString" <> zmString <> "(" <> show lineString <> ")"
        where
            LineString line = lineString
            first = head line
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Valid (LineString a) where
    isValid (LineString []) = True
    isValid (LineString (fpoint:tpoints)) = all (==fpointDimension) tpointDimensions
        where
            fpointDimension = pointDimension fpoint
            tpointDimensions = pointDimension <$> tpoints
