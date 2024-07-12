{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Data.Wtk.Types (Point(..), LineString, Polygon) where

import Data.Wtk.Classes (Valid(..))
import Data.List (sort, group)
import Data.Maybe (isJust, isNothing)

-- Primitives
data Point = Point{
    x :: Int,
    y :: Int,
    z :: Maybe Int,
    m :: Maybe Int
}
    deriving (Eq, Ord)
instance Valid Point where
    isValid (Point _ _ z' m') = isNothing m' || isJust z'

newtype LineString = LineString{
    line :: [Point]
}
    deriving Eq
instance Valid LineString where
    isValid (LineString []) = True
    isValid (LineString (fpoint:tpoints)) = all (==fpointDimension) tpointDimensions
        where
            fpointDimension = pointDimension fpoint
            tpointDimensions = pointDimension <$> tpoints

newtype Polygon = Polygon{
    polygon :: [LineString]
}
instance Valid Polygon where
    isValid (Polygon lines') = firstPoint == lastPoint
        where
            firstPoint = head lines'
            lastPoint = head lines'

newtype Triangle = Triangle{
    triangle :: [LineString]
}
instance Valid Triangle where
    isValid (Triangle lines') = firstPoint == lastPoint && size == 4
        where
            firstPoint = head  lines'
            lastPoint = head lines'
            size = length lines'


data Primitives = PrimPoint Point | PrimLine LineString | PrimPolygon Polygon | PrimTriangle Triangle
instance Valid Primitives where
    isValid (PrimPoint a)    = isValid a
    isValid (PrimLine a)     = isValid a
    isValid (PrimPolygon a)  = isValid a
    isValid (PrimTriangle a) = isValid a

-- Multipart
newtype MultiPoint = MultiPoint{
    points :: [Point]
}
instance Valid MultiPoint where
    isValid (MultiPoint points') = all isValid points' 

newtype MultiLineString = MultiLineString{
    lines :: [LineString]
}
instance Valid MultiLineString where
    isValid (MultiLineString lines') = all isValid lines' 

newtype MultiPolygon = MultiPolygon{
    polygons :: [Polygon]
}
instance Valid MultiPolygon where
    isValid (MultiPolygon polygons') = all isValid polygons' 

newtype TIN = TIN{
    triangles :: [Triangle]
}
instance Valid TIN where
    isValid (TIN triangles') = validTriangles && isContinuous
        where
            validTriangles = all isValid triangles'
            allPoints = concatMap ( concatMap line . triangle) triangles'
            isContinuous =  notElem 1 $ length <$> group (sort allPoints) -- checks if every point is on at least two triangles.

newtype GeometryCollection = GeometryCollection{
    collection :: [Primitives]
}
instance Valid GeometryCollection where
    isValid (GeometryCollection collection') = all isValid collection'

-- Helpers
pointDimension :: Point -> Int
pointDimension (Point _ _ z' m')
    | isJust m' = 4
    | isJust z' = 3 
    | otherwise = 2