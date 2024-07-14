module Data.Wkt.Validators (module Data.Wkt.Validators) where
import Data.Wkt.Types
import Data.List (sort, group)
import Data.Maybe (isJust)

class Valid a where
    isValid :: a -> Bool

-- Primitives
instance Valid (Point a) where
    isValid (Point {}) = True

instance Valid (LineString a) where
    isValid (LineString []) = True
    isValid (LineString (fpoint:tpoints)) = all (==fpointDimension) tpointDimensions
        where
            fpointDimension = pointDimension fpoint
            tpointDimensions = pointDimension <$> tpoints

instance Eq a => Valid (Polygon a) where
    isValid (Polygon lines') = validLines && validPolygon
        where
            validLines = all isValid lines'
            validPolygon = all (\(LineString line) -> head line == last line) lines'

instance Eq a => Valid (Triangle a) where
    isValid (Triangle lines') = firstPoint == lastPoint && size == 4
        where
            firstPoint = head  lines'
            lastPoint = head lines'
            size = length lines'

instance Eq a => Valid (Primitives a) where
    isValid (PrimPoint a)    = isValid a
    isValid (PrimLine a)     = isValid a
    isValid (PrimPolygon a)  = isValid a
    isValid (PrimTriangle a) = isValid a

-- Multipart
instance Valid (MultiPoint a) where
    isValid (MultiPoint points') = all isValid points'

instance Valid (MultiLineString a) where
    isValid (MultiLineString lines') = all isValid lines'

instance Eq a => Valid (MultiPolygon a) where
    isValid (MultiPolygon polygons') = all isValid polygons'

instance Ord a => Valid (TIN a) where
    isValid (TIN triangles) = validTriangles && isContinuous
        where
            validTriangles = all isValid triangles
            allPoints = concatMap (\(Triangle points) -> points) triangles
            isContinuous =  notElem 1 $ length <$> group (sort allPoints) -- checks if every point is on at least two sides.

instance Eq a => Valid (GeometryCollection a) where
    isValid (GeometryCollection collection') = all isValid collection'

instance Ord a => Valid (PolyhedralSurface a) where
    isValid (PolyhedralSurface surfaces) = valid -- checks if every side has two triangles associated to it.
        where
            sides = group $ sort $ concatMap allSides surfaces
            valid =  all ((==2) . length) sides
-- Helpers
pointDimension :: Point a -> Int
pointDimension (Point _ _ z' m')
    | isJust m' = 4
    | isJust z' = 3
    | otherwise = 2

-- Must be valid Triangle
allSides :: Triangle a-> [(Point a, Point a)]
allSides (Triangle vertices) = allPairs vertices

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs [_] = []
allPairs (x1:x2:xs) = (x1,x2) : allPairs (x2:xs)