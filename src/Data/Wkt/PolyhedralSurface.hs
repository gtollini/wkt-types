{-# LANGUAGE OverloadedStrings#-}
module Data.Wkt.PolyhedralSurface (module Data.Wkt.PolyhedralSurface) where

import Data.Wkt.Classes
import Data.Wkt.Triangle
import Data.List (intercalate, group, sort)
import Data.Wkt.Point
import Data.Wkt.Helpers (generateZMString)
import Data.Text (pack)


newtype PolyhedralSurface a = PolyhedralSurface [Triangle a]

instance Show a => Show (PolyhedralSurface a) where
    show (PolyhedralSurface surface) = intercalate ", " surface'
        where
            surface' = map (\(Triangle triangle) -> "(" <> intercalate ", " (show <$> triangle) <> ")") surface

instance Show a => ToWKT (PolyhedralSurface a) where
    toWKT (PolyhedralSurface surface) = "PolyhedralSurface" <> zmString <> "(" <> pack (show surface) <> ")"
        where
            (Triangle firstTriangle) = head surface
            first = head firstTriangle
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Ord a => Valid (PolyhedralSurface a) where
    isValid (PolyhedralSurface surfaces) = valid -- checks if every side has two triangles associated to it.
        where
            sides = group $ sort $ concatMap allSides surfaces
            valid =  all ((==2) . length) sides
