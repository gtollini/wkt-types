module Data.Wkt.MultiLineString (module Data.Wkt.MultiLineString) where

import Data.Wkt.Classes
import Data.Wkt.LineString
import Data.List (intercalate)
import Data.Wkt.Helpers (showP, generateZMString)
import Data.Wkt.Point

newtype MultiLineString a = MultiLineString [LineString a]

instance Show a => Show (MultiLineString a) where
    show (MultiLineString lineStrings) = intercalate ", " lines'
        where
            lines' = showP <$> lineStrings

instance Show a => ToWKT (MultiLineString a) where
    toWKT multiLineString = "MultiLineString" <> zmString <> "(" <> show multiLineString <> ")"
        where
            (MultiLineString lineStrings) = multiLineString
            (LineString firstLine) = head lineStrings
            first = head firstLine
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Valid (MultiLineString a) where
    isValid (MultiLineString lines') = all isValid lines'
