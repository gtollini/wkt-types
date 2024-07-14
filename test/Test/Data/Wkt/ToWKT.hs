module Test.Data.Wkt.ToWKT (module Test.Data.Wkt.ToWKT) where
    
import Test.Hspec
-- import Test.QuickCheck
import Test.Data.Wkt.TestElements

import Data.Wkt.Classes
-- Primitives
points :: Spec
points = do
    describe "ToWKT Points" $ do
        it "should convert a 2D Point to WKT" $ do
            toWKT point `shouldBe` "Point (1 2)"
        it "should convert a 3D (Z) Point to WKT" $ do
            toWKT pointZ `shouldBe` "Point Z (1 2 3)"
        it "should convert a 3D (M) Point to WKT" $ do
            toWKT pointM `shouldBe` "Point M (1 2 4)"
        it "should convert a 4D Point to WKT" $ do
            toWKT pointZM `shouldBe` "Point ZM (1 2 3 4)"
    -- TODO: doubles, with e-notation

lineStrings :: Spec
lineStrings = do
    describe "ToWKT LineStrings" $ do
        it "should convert a 2D LineString to WKT" $ do
            toWKT lineString   `shouldBe` "LineString (1 2, 1 2)"
        it "should convert a 3D (Z) LineString to WKT" $ do
            toWKT lineStringZ  `shouldBe` "LineString Z (1 2 3, 1 2 3)"
        it "should convert a 3D (M) LineString to WKT" $ do
            toWKT lineStringM  `shouldBe` "LineString M (1 2 4, 1 2 4)"
        it "should convert a 4D LineString to WKT" $ do
            toWKT lineStringZM `shouldBe` "LineString ZM (1 2 3 4, 1 2 3 4)"

polygons :: Spec
polygons = do
    describe "ToWKT Polygons" $ do
        it "should convert a 2D Polygon to WKT" $ do
            toWKT polygon  `shouldBe` "Polygon ((1 2, 1 2))"
        it "should convert a 3D (Z) Polygon to WKT" $ do
            toWKT polygonZ  `shouldBe` "Polygon Z ((1 2 3, 1 2 3))"
        it "should convert a 3D (M) Polygon to WKT" $ do
            toWKT polygonM  `shouldBe` "Polygon M ((1 2 4, 1 2 4))"
        it "should convert a 4D Polygon to WKT" $ do
            toWKT polygonZM `shouldBe` "Polygon ZM ((1 2 3 4, 1 2 3 4))"
        it "should convert a 2D Polygon with a hole to WKT" $ do
            toWKT polygonR  `shouldBe` "Polygon ((1 2, 1 2), (1 2, 1 2))"

-- Multipart
multiPoints :: Spec
multiPoints = do 
    describe "ToWKT MultiPoints" $ do
        it "should convert a 2D MultiPoint to WKT" $ do
            toWKT multiPoint   `shouldBe` "MultiPoint ((1 2), (1 2))"
        it "should convert a 3D (Z) MultiPoint to WKT" $ do
            toWKT multiPointZ  `shouldBe` "MultiPoint Z ((1 2 3), (1 2 3))"
        it "should convert a 3D (M) MultiPoint to WKT" $ do
            toWKT multiPointM  `shouldBe` "MultiPoint M ((1 2 4), (1 2 4))"
        it "should convert a 4D MultiPoint to WKT" $ do
            toWKT multiPointZM `shouldBe` "MultiPoint ZM ((1 2 3 4), (1 2 3 4))"

multiLineStrings :: Spec
multiLineStrings = do
    describe "ToWKT MultiLineStrings" $ do
        it "should convert a 2D MultiLineString to WKT" $ do
            toWKT multiLineString   `shouldBe` "MultiLineString ((1 2, 1 2), (1 2, 1 2))"
        it "should convert a 3D (Z) MultiLineString to WKT" $ do
            toWKT multiLineStringZ  `shouldBe` "MultiLineString Z ((1 2 3, 1 2 3), (1 2 3, 1 2 3))"
        it "should convert a 3D (M) MultiLineString to WKT" $ do
            toWKT multiLineStringM  `shouldBe` "MultiLineString M ((1 2 4, 1 2 4), (1 2 4, 1 2 4))"
        it "should convert a 4D MultiLineString to WKT" $ do
            toWKT multiLineStringZM `shouldBe` "MultiLineString ZM ((1 2 3 4, 1 2 3 4), (1 2 3 4, 1 2 3 4))"

multiPolygons :: Spec
multiPolygons = do
    describe "ToWKT MultiPolygons" $ do
        it "should convert a 2D MultiPolygon to WKT" $ do
            toWKT multiPolygon   `shouldBe` "MultiPolygon (((1 2, 1 2)), ((1 2, 1 2)))"
        it "should convert a 3D (Z) MultiPolygon to WKT" $ do
            toWKT multiPolygonZ  `shouldBe` "MultiPolygon Z (((1 2 3, 1 2 3)), ((1 2 3, 1 2 3)))"
        it "should convert a 3D (M) MultiPolygon to WKT" $ do
            toWKT multiPolygonM  `shouldBe` "MultiPolygon M (((1 2 4, 1 2 4)), ((1 2 4, 1 2 4)))"
        it "should convert a 4D MultiPolygon to WKT" $ do
            toWKT multiPolygonZM `shouldBe` "MultiPolygon ZM (((1 2 3 4, 1 2 3 4)), ((1 2 3 4, 1 2 3 4)))"

geometryCollections :: Spec
geometryCollections = do
    describe "ToWKT GeometryCollections" $ do
        it "should convert a homogenous GeometryCollection to WKT" $ do
            toWKT geometryCollectionHomo   `shouldBe` "GeometryCollection (Point (1 2), Point (1 2))"
        it "should convert a heterogenous GeometryCollection to WKT" $ do
            toWKT geometryCollectionHetero `shouldBe` "GeometryCollection (Point (1 2), LineString (1 2, 1 2))"