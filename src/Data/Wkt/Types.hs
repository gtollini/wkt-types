{-# LANGUAGE TypeSynonymInstances #-}
module Data.Wkt.Types (module Data.Wkt.Types) where

-- Primitives
data Point a = Point{
    x :: a,
    y :: a,
    z :: Maybe a,
    m :: Maybe a
}
    deriving (Eq, Ord)

newtype LineString a = LineString [Point a]

newtype Triangle a  = Triangle [Point a]

newtype Polygon a = Polygon [LineString a]

data Primitives a = PrimPoint (Point a)| PrimLine (LineString a)| PrimPolygon (Polygon a)| PrimTriangle (Triangle a)

-- Multipart
newtype MultiPoint a = MultiPoint [Point a]

newtype MultiLineString a = MultiLineString [LineString a]

newtype MultiPolygon a = MultiPolygon [Polygon a]


newtype PolyhedralSurface a = PolyhedralSurface [Triangle a]

newtype TIN a = TIN [Triangle a]

newtype GeometryCollection a =  GeometryCollection [Primitives a]