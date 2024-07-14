module Data.Wkt.MultiPoint (module Data.Wkt.MultiPoint) where

import Data.Wkt.Classes
import Data.Wkt.Point
import Data.Wkt.Helpers (generateZMString, showP)
import Data.List (intercalate)

newtype MultiPoint a = MultiPoint [Point a]

instance Show a => Show (MultiPoint a) where
    show (MultiPoint points) = intercalate ", " (showP <$> points)

instance Show a => ToWKT (MultiPoint a) where
    toWKT multiPoint = "MultiPoint" <> zmString <> "(" <> show multiPoint <> ")"
        where
            (MultiPoint points) = multiPoint
            first = head points
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Valid (MultiPoint a) where
    isValid (MultiPoint points') = all isValid points'