{-# LANGUAGE TypeSynonymInstances #-}
module Data.Wtk.Types where

-- Primitives
data Point = Point{
    x :: Double,
    y :: Double,
    z :: Maybe Double,
    m :: Maybe Double
}
    deriving (Eq, Ord)

newtype LineString = LineString{
    line :: [Point]
}
    deriving Eq

newtype Triangle = Triangle{
    vertices :: [Point]
}
newtype Polygon = Polygon{
    polygon :: [LineString]
}

data Primitives = PrimPoint Point | PrimLine LineString | PrimPolygon Polygon | PrimTriangle Triangle

-- Multipart
newtype MultiPoint = MultiPoint{
    points :: [Point]
}

newtype MultiLineString = MultiLineString{
    lines :: [LineString]
}

newtype MultiPolygon = MultiPolygon{
    polygons :: [Polygon]
}

newtype PolyhedralSurface = PolyhedralSurface{
    surfaces :: [Triangle]
}
newtype TIN = TIN{
    triangles :: [Triangle]
}

newtype GeometryCollection = GeometryCollection{
    collection :: [Primitives]
}
