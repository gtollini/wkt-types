module Test.Data.Wkt.TestElements (module Test.Data.Wkt.TestElements) where

import Data.Wkt.Types

-- Primitives
-- Points
point :: Point Int
point = Point {x=1, y=2, z=Nothing, m=Nothing}

pointZ :: Point Int
pointZ = Point {x=1, y=2, z=Just 3, m=Nothing}

pointM :: Point Int
pointM = Point {x=1, y=2, z=Nothing, m=Just 4}

pointZM :: Point Int
pointZM = Point {x=1, y=2, z=Just 3, m=Just 4}

-- LineStrings
lineString :: LineString Int
lineString = LineString [point, point]

lineStringZ :: LineString Int
lineStringZ = LineString [pointZ, pointZ]

lineStringM :: LineString Int
lineStringM = LineString [pointM, pointM]

lineStringZM :: LineString Int
lineStringZM = LineString [pointZM, pointZM]

-- Triangles (TODO)
-- triangle :: Triangle Int
-- triangle = Triangle [point, point, point, point]

-- triangleZ :: Triangle Int
-- triangleZ = Triangle [pointZ, pointZ, pointZ, pointZ]

-- triangleM :: Triangle Int
-- triangleM = Triangle [pointM, pointM, pointM, pointM]

-- triangleZM :: Triangle Int
-- triangleZM = Triangle [pointZM, pointZM, pointZM, pointZM]

-- Polygons
polygon :: Polygon Int
polygon = Polygon [lineString]

polygonZ :: Polygon Int
polygonZ = Polygon [lineStringZ]

polygonM :: Polygon Int
polygonM = Polygon [lineStringM]

polygonZM :: Polygon Int
polygonZM = Polygon [lineStringZM]

polygonR :: Polygon Int
polygonR = Polygon [lineString, lineString]

-- Multiparts
-- MultiPoints
multiPoint :: MultiPoint Int
multiPoint = MultiPoint [point, point]

multiPointZ :: MultiPoint Int
multiPointZ = MultiPoint [pointZ, pointZ]

multiPointM :: MultiPoint Int
multiPointM = MultiPoint [pointM, pointM]

multiPointZM :: MultiPoint Int
multiPointZM = MultiPoint [pointZM, pointZM]

-- MultiLineStrings
multiLineString :: MultiLineString Int
multiLineString = MultiLineString [lineString, lineString]

multiLineStringZ :: MultiLineString Int
multiLineStringZ = MultiLineString [lineStringZ, lineStringZ]

multiLineStringM :: MultiLineString Int
multiLineStringM = MultiLineString [lineStringM, lineStringM]

multiLineStringZM :: MultiLineString Int
multiLineStringZM = MultiLineString [lineStringZM, lineStringZM]

-- MultiPolygons
multiPolygon :: MultiPolygon Int
multiPolygon = MultiPolygon [polygon, polygon]

multiPolygonZ :: MultiPolygon Int
multiPolygonZ = MultiPolygon [polygonZ, polygonZ]

multiPolygonM :: MultiPolygon Int
multiPolygonM = MultiPolygon [polygonM, polygonM]

multiPolygonZM :: MultiPolygon Int
multiPolygonZM = MultiPolygon [polygonZM, polygonZM]

-- PolyhedralSurface (TODO)

-- TIN (TODO)

-- GeometryCollection
geometryCollectionHomo :: GeometryCollection Int
geometryCollectionHomo = GeometryCollection [PrimPoint point, PrimPoint point]

geometryCollectionHetero :: GeometryCollection Int
geometryCollectionHetero = GeometryCollection [PrimPoint point, PrimLine lineString]