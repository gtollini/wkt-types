{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Wkt.Showables (module Data.Wkt.Showables) where

import Data.Wkt.Types
import Data.Maybe (isJust)
import Data.List (intercalate)

-- Primitives
instance Show a => Show (Point a) where
    show (Point {x,y,z,m}) = pointValue
        where
            -- TODO: check if it's okay to have . or if I must always e.
            x' = show x
            y' = show y
            z' = maybe "" ((" " <>). show) z
            m' = maybe "" ((" " <>). show) m
            pointValue = x' <> " " <> y' <> z' <> m'

instance Show a => Show (LineString a) where
    show (LineString line) = intercalate ", " (show <$> line)

instance Show a => Show (Triangle a) where
    show (Triangle vertices) = intercalate ", " (show <$> vertices)

instance Show a => Show (Polygon a) where
    show (Polygon polygon) = intercalate ", " rings
        where
            rings = map (\(LineString ring) -> "(" <> intercalate ", " (show <$> ring) <> ")") polygon

instance Show a => Show (Primitives a) where
    show (PrimPoint a)    = show a
    show (PrimLine a)     = show a
    show (PrimPolygon a)  = show a
    show (PrimTriangle a) = show a

-- Multipart
instance Show a => Show (MultiPoint a) where
    show (MultiPoint points) = intercalate ", " (showP <$> points)

instance Show a => Show (MultiLineString a) where
    show (MultiLineString lineStrings) = intercalate ", " lines'
        where
            lines' = showP <$> lineStrings
instance Show a => Show (MultiPolygon a) where
    show (MultiPolygon polygons) = intercalate ", " polygons'
        where
            polygons' =showP <$> polygons
            -- polygons' = map (\(Polygon polygon) -> "(" <> intercalate ", " (show <$> polygon) <> ")") polygons

instance Show a => Show (PolyhedralSurface a) where
    show (PolyhedralSurface surface) = intercalate ", " surface'
        where
            surface' = map (\(Triangle triangle) -> "(" <> intercalate ", " (show <$> triangle) <> ")") surface

instance Show a => Show (TIN a) where
    show (TIN triangles) = intercalate ", " triangles'
        where
            triangles' = map (\(Triangle triangle) -> "(" <> intercalate ", " (show <$> triangle) <> ")") triangles

instance Show a => Show (GeometryCollection a) where
    show (GeometryCollection collection) = intercalate ", " (show <$> collection)   

class ToWKT a where
    toWKT :: a -> String

instance Show a => ToWKT (Point a) where
    toWKT point = "Point" <> zmString <> "(" <> show point <> ")"
        where
            Point{z,m} = point
            zmString
                |isJust z && isJust m = " ZM "
                |isJust z = " Z "
                |isJust m = " M "
                |otherwise = " "

instance Show a => ToWKT (LineString a) where
    toWKT lineString
        | null line = "EMPTY"
        | otherwise = "LineString" <> zmString <> "(" <> show lineString <> ")"
        where
            LineString line = lineString
            first = head line
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "

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


instance Show a => ToWKT (Polygon a) where
    toWKT polygon = "Polygon" <> zmString <> "(" <> show polygon <> ")"
        where
            Polygon rings = polygon
            (LineString firstLine) = head rings
            first = head firstLine
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "

instance Show a => ToWKT (Primitives a) where
    toWKT (PrimPoint a)    = toWKT a
    toWKT (PrimLine a)     = toWKT a
    toWKT (PrimPolygon a)  = toWKT a
    toWKT (PrimTriangle a) = toWKT a

-- Multipart
instance Show a => ToWKT (MultiPoint a) where
    toWKT multiPoint = "MultiPoint" <> zmString <> "(" <> show multiPoint <> ")"
        where
            (MultiPoint points) = multiPoint
            first = head points
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "

instance Show a => ToWKT (MultiLineString a) where
    toWKT multiLineString = "MultiLineString" <> zmString <> "(" <> show multiLineString <> ")"
        where
            (MultiLineString lineStrings) = multiLineString
            (LineString firstLine) = head lineStrings
            first = head firstLine
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "


instance Show a => ToWKT (MultiPolygon a) where
    toWKT multiPolygon = "MultiPolygon" <> zmString <> "(" <> show multiPolygon <> ")"
        where
            (MultiPolygon polygons) = multiPolygon
            (Polygon firstPolygon) = head polygons
            (LineString firstLine) = head firstPolygon
            first = head firstLine
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "

instance Show a => ToWKT (PolyhedralSurface a) where
    toWKT (PolyhedralSurface surface) = "PolyhedralSurface" <> zmString <> "(" <> show surface <> ")"
        where
            (Triangle firstTriangle) = head surface
            first = head firstTriangle
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "

instance Show a => ToWKT (TIN a) where
    toWKT (TIN triangles) = "TIN" <> zmString <> "(" <> show triangles <> ")"
        where
            (Triangle firstTriangle) = head triangles
            first = head firstTriangle
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "

instance Show a => ToWKT (GeometryCollection a) where
    toWKT (GeometryCollection collection) = "GeometryCollection" <> zmString <> "(" <> show collection <> ")"
        where
            first = case head collection of
                PrimPoint a                 -> a
                PrimLine (LineString a)     -> head a
                PrimTriangle (Triangle a)   -> head a
                PrimPolygon (Polygon a)     -> (\(LineString a') -> head a') $ head a
            z' = z first
            m' = m first

            zmString
                |isJust z' && isJust m' = " ZM "
                |isJust z' = " Z "
                |isJust m' = " M "
                |otherwise = " "

-- Helpers
showP :: Show a => a -> String
showP = ("(" <>) . (<> ")") . show