{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Wtk.Showables (module Data.Wtk.Showables) where

import Data.Wtk.Types
import Data.Maybe (isJust)
import Data.List (intersperse, intercalate)

-- Tests
a = Point{x=1,y=2,z=Just 3,m=Just 4}
b = Point{x=1,y=2,z=Nothing,m=Just 4}
c = Point{x=1,y=2,z=Just 3,m=Nothing}
d = Point{x=1,y=2,z=Nothing,m=Nothing}


a1 = LineString[a,a,a]
b1 = LineString[b,b,b]
c1 = LineString[c,c,c]
d1 = LineString[d,d,d]
e1 = LineString[]

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
    show (Polygon polygon) = undefined

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