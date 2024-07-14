{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Wtk.Showables (module Data.Wtk.Showables) where

import Data.Wtk.Types
import Data.Maybe (isJust)
import Data.List (intercalate)

-- Primitives
instance Show Point where
    show (Point {x,y,z,m}) = pointValue
        where
            -- TODO: check if it's okay to have . or if I must always e.
            x' = show x
            y' = show y
            z' = maybe "" ((" " <>). show) z
            m' = maybe "" ((" " <>). show) m
            pointValue = x' <> " " <> y' <> z' <> m'

instance Show LineString where
    show (LineString line) = intercalate ", " (show <$> line)

instance Show Triangle where
    show (Triangle vertices) = intercalate ", " (show <$> vertices)

instance Show Polygon where
    show (Polygon polygon) = intercalate ", " rings
        where
            rings = map (\(LineString ring) -> "(" <> intercalate ", " (show <$> ring) <> ")") polygon

instance Show Primitives where
    show (PrimPoint a)    = show a
    show (PrimLine a)     = show a
    show (PrimPolygon a)  = show a
    show (PrimTriangle a) = show a

-- Multipart
instance Show MultiPoint where
    show (MultiPoint points) = intercalate ", " (show <$> points)

instance Show MultiLineString where
    show (MultiLineString lineStrings) = intercalate ", " lines'
        where
            lines' = map (\(LineString line) -> "(" <> intercalate ", " (show <$> line) <> ")") lineStrings
instance Show MultiPolygon where
    show (MultiPolygon polygons) = intercalate ", " polygons'
        where
            polygons' = map (\(Polygon polygon) -> "(" <> intercalate ", " (show <$> polygon) <> ")") polygons

instance Show PolyhedralSurface where
    show (PolyhedralSurface surface) = intercalate ", " surface'
        where
            surface' = map (\(Triangle triangle) -> "(" <> intercalate ", " (show <$> triangle) <> ")") surface

instance Show TIN where
    show (TIN triangles) = intercalate ", " triangles'
        where
            triangles' = map (\(Triangle triangle) -> "(" <> intercalate ", " (show <$> triangle) <> ")") triangles

instance Show GeometryCollection where
    show (GeometryCollection collection) = intercalate ", " (show <$> collection)   

class ToWKT a where
    toWKT :: a -> String

instance ToWKT Point where
    toWKT point = "Point " <> zString <> mString <> " (" <> show point <> ")"
        where
            Point{z,m} = point

            zString = if isJust z then "Z" else ""
            mString = if isJust m then "M" else ""

instance ToWKT LineString where
    toWKT lineString
        | null line = "EMPTY"
        | otherwise = "LineString " <> zString <> mString <> " (" <> show lineString <> ")"
        where
            LineString line = lineString
            first = head line
            z' = z first
            m' = m first

            zString = if isJust z' then "Z" else ""
            mString = if isJust m' then "M" else ""