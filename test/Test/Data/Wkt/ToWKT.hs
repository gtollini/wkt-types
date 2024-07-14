module Test.Data.Wkt.ToWKT (module Test.Data.Wkt.ToWKT) where
    
import Test.Hspec
-- import Test.QuickCheck
import Data.Wkt.Types
import Data.Wkt.Showables
import Test.Data.Wkt.TestElements

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

-- multiPolygon :: Spec
-- multiPolygon = do
--     describe "ToWKT multiPolygon" $ do
--         it "should convert a 2D MultiPolygon to WKT" $ do
--             toWKT (MultiPolygon [Polygon [LineString [Point {x=1, y=2, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}, Point {x=5, y=6, z=Nothing, m=Nothing}, Point {x=1, y=2, z=Nothing, m=Nothing}], LineString [Point {x=2, y=3, z=Nothing, m=Nothing}, Point {x=4, y=5, z=Nothing, m=Nothing}, Point {x=6, y=7, z=Nothing, m=Nothing}, Point {x=2, y=3, z=Nothing, m=Nothing}]]] :: MultiPolygon Int) 
--                 `shouldBe` "MultiPolygon (((1 2, 3 4, 5 6, 1 2), (2 3, 4 5, 6 7, 2 3)))"
--         it "should convert a 3D (Z) MultiPolygon to WKT" $ do
--             toWKT (MultiPolygon [Polygon [LineString [Point {x=1, y=2, z=Just 3, m=Nothing}, Point {x=3, y=4, z=Just 5, m=Nothing}, Point {x=5, y=6, z=Just 7, m=Nothing}, Point {x=1, y=2, z=Just 3, m=Nothing}], LineString [Point {x=2, y=3, z=Just 4, m=Nothing}, Point {x=4, y=5, z=Just 6, m=Nothing}, Point {x=6, y=7, z=Just 8, m=Nothing}, Point {x=2, y=3, z=Just 4, m=Nothing}]]] :: MultiPolygon Int) 
--                 `shouldBe` "MultiPolygon Z (((1 2 3, 3 4 5, 5 6 7, 1 2 3), (2 3 4, 4 5 6, 6 7 8, 2 3 4)))"
--         it "should convert a 3D (M) MultiPolygon to WKT" $ do
--             toWKT (MultiPolygon [Polygon [LineString [Point {x=1, y=2, z=Nothing, m=Just 4}, Point {x=3, y=4, z=Nothing, m=Just 5}, Point {x=5, y=6, z=Nothing, m=Just 6}, Point {x=1, y=2, z=Nothing, m=Just 4}], LineString [Point {x=2, y=3, z=Nothing, m=Just 5}, Point {x=4, y=5, z=Nothing, m=Just 6}, Point {x=6, y=7, z=Nothing, m=Just 7}, Point {x=2, y=3, z=Nothing, m=Just 5}]]] :: MultiPolygon Int) 
--                 `shouldBe` "MultiPolygon M (((1 2 4, 3 4 5, 5 6 6, 1 2 4), (2 3 5, 4 5 6, 6 7 7, 2 3 5)))"
--         it "should convert a 4D MultiPolygon to WKT" $ do
--             toWKT (MultiPolygon [Polygon [LineString [Point {x=1, y=2, z=Just 3, m=Just 4}, Point {x=3, y=4, z=Just 5, m=Just 6}, Point {x=5, y=6, z=Just 7, m=Just 8}, Point {x=1, y=2, z=Just 3, m=Just 4}], LineString [Point {x=2, y=3, z=Just 4, m=Just 5}, Point {x=4, y=5, z=Just 6, m=Just 7}, Point {x=6, y=7, z=Just 8, m=Just 9}, Point {x=2, y=3, z=Just 4, m=Just 5}]]] :: MultiPolygon Int) 
--                 `shouldBe` "MultiPolygon ZM (((1 2 3 4, 3 4 5 6, 5 6 7 8, 1 2 3 4), (2 3 4 5, 4 5 6 7, 6 7 8 9, 2 3 4 5)))"
--         it "should convert a 2D MultiPolygon with a hole to WKT" $ do
--             toWKT (MultiPolygon [Polygon [LineString [Point {x=1, y=2, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}, Point {x=5, y=6, z=Nothing, m=Nothing}, Point {x=1, y=2, z=Nothing, m=Nothing}], LineString [Point {x=2, y=3, z=Nothing, m=Nothing}, Point {x=4, y=5, z=Nothing, m=Nothing}, Point {x=6, y=7, z=Nothing, m=Nothing}, Point {x=2, y=3, z=Nothing, m=Nothing}], LineString [Point {x=3, y=4, z=Nothing, m=Nothing}, Point {x=5, y=6, z=Nothing, m=Nothing}, Point {x=7, y=8, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}]]] :: MultiPolygon Int) 
--                 `shouldBe` "MultiPolygon (((1 2, 3 4, 5 6, 1 2), (2 3, 4 5, 6 7, 2 3), (3 4, 5 6, 7 8, 3 4)))"
--         it "should convert a MultiPolygon with multiple polygons" $ do
--             toWKT (MultiPolygon [Polygon [LineString [Point {x=1, y=2, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}, Point {x=5, y=6, z=Nothing, m=Nothing}, Point {x=1, y=2, z=Nothing, m=Nothing}], LineString [Point {x=2, y=3, z=Nothing, m=Nothing}, Point {x=4, y=5, z=Nothing, m=Nothing}, Point {x=6, y=7, z=Nothing, m=Nothing}, Point {x=2, y=3, z=Nothing, m=Nothing}], LineString [Point {x=3, y=4, z=Nothing, m=Nothing}, Point {x=5, y=6, z=Nothing, m=Nothing}, Point {x=7, y=8, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}]], Polygon [LineString [Point {x=1, y=2, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}, Point {x=5, y=6, z=Nothing, m=Nothing}, Point {x=1, y=2, z=Nothing, m=Nothing}], LineString [Point {x=2, y=3, z=Nothing, m=Nothing}, Point {x=4, y=5, z=Nothing, m=Nothing}, Point {x=6, y=7, z=Nothing, m=Nothing}, Point {x=2, y=3, z=Nothing, m=Nothing}], LineString [Point {x=3, y=4, z=Nothing, m=Nothing}, Point {x=5, y=6, z=Nothing, m=Nothing}, Point {x=7, y=8, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}]]] :: MultiPolygon Int) 
--                 `shouldBe` "MultiPolygon (((1 2, 3 4, 5 6, 1 2), 