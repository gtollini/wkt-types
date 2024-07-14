{-# LANGUAGE TypeSynonymInstances #-}
module Data.Wtk.Types (module Data.Wtk.Types) where

-- Primitives
data Point = Point{
    x :: Double,
    y :: Double,
    z :: Maybe Double,
    m :: Maybe Double
}
    deriving (Eq, Ord)

newtype LineString = LineString [Point]

newtype Triangle  = Triangle [Point]

newtype Polygon = Polygon [LineString]

data Primitives = PrimPoint Point | PrimLine LineString | PrimPolygon Polygon | PrimTriangle Triangle

-- Multipart
newtype MultiPoint = MultiPoint [Point]

newtype MultiLineString = MultiLineString [LineString]

newtype MultiPolygon = MultiPolygon [Polygon]


newtype PolyhedralSurface = PolyhedralSurface [Triangle]

newtype TIN = TIN [Triangle]

newtype GeometryCollection =  GeometryCollection [Primitives]