module Test.Data.Wkt.ToWKT (module Test.Data.Wkt.ToWKT) where
    
import Test.Hspec
-- import Test.QuickCheck
import Data.Wkt.Types
import Data.Wkt.Showables

points :: Spec
points = do
    describe "ToWKT Points" $ do
        it "should convert a 2D Point to WKT" $ do
            toWKT (Point {x=1, y=2, z=Nothing, m=Nothing} :: Point Int) `shouldBe` "Point (1 2)"
        it "should convert a 3D (Z) Point to WKT" $ do
            toWKT (Point {x=1, y=2, z=Just 3, m=Nothing} :: Point Int) `shouldBe` "Point Z (1 2 3)"
        it "should convert a 3D (M) Point to WKT" $ do
            toWKT (Point {x=1, y=2, z=Nothing, m=Just 4} :: Point Int) `shouldBe` "Point M (1 2 4)"
        it "should convert a 4D Point to WKT" $ do
            toWKT (Point {x=1, y=2, z=Just 3, m=Just 4} :: Point Int) `shouldBe` "Point ZM (1 2 3 4)"
    -- TODO: doubles, with e-notation

lineStrings :: Spec
lineStrings = do
    describe "ToWKT LineStrings" $ do
        it "should convert a 2D LineString to WKT" $ do
            toWKT (LineString [Point {x=1, y=2, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}] :: LineString Int) 
                `shouldBe` "LineString (1 2, 3 4)"
        it "should convert a 3D (Z) LineString to WKT" $ do
            toWKT (LineString [Point {x=1, y=2, z=Just 3, m=Nothing}, Point {x=3, y=4, z=Just 5, m=Nothing}] :: LineString Int) 
                `shouldBe` "LineString Z (1 2 3, 3 4 5)"
        it "should convert a 3D (M) LineString to WKT" $ do
            toWKT (LineString [Point {x=1, y=2, z=Nothing, m=Just 4}, Point {x=3, y=4, z=Nothing, m=Just 5}] :: LineString Int) 
                `shouldBe` "LineString M (1 2 4, 3 4 5)"
        it "should convert a 4D LineString to WKT" $ do
            toWKT (LineString [Point {x=1, y=2, z=Just 3, m=Just 4}, Point {x=3, y=4, z=Just 5, m=Just 6}] :: LineString Int) 
                `shouldBe` "LineString ZM (1 2 3 4, 3 4 5 6)"

polygons :: Spec
polygons = do
    describe "ToWKT Polygons" $ do
        it "should convert a 2D Polygon to WKT" $ do
            toWKT (Polygon [LineString [Point {x=1, y=2, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}, Point {x=5, y=6, z=Nothing, m=Nothing}, Point {x=1, y=2, z=Nothing, m=Nothing}]] :: Polygon Int) 
                `shouldBe` "Polygon ((1 2, 3 4, 5 6, 1 2))"
        it "should convert a 3D (Z) Polygon to WKT" $ do
            toWKT (Polygon [LineString [Point {x=1, y=2, z=Just 3, m=Nothing}, Point {x=3, y=4, z=Just 5, m=Nothing}, Point {x=5, y=6, z=Just 7, m=Nothing}, Point {x=1, y=2, z=Just 3, m=Nothing}]] :: Polygon Int) 
                `shouldBe` "Polygon Z ((1 2 3, 3 4 5, 5 6 7, 1 2 3))"
        it "should convert a 3D (M) Polygon to WKT" $ do
            toWKT (Polygon [LineString [Point {x=1, y=2, z=Nothing, m=Just 4}, Point {x=3, y=4, z=Nothing, m=Just 5}, Point {x=5, y=6, z=Nothing, m=Just 6}, Point {x=1, y=2, z=Nothing, m=Just 4}]] :: Polygon Int) 
                `shouldBe` "Polygon M ((1 2 4, 3 4 5, 5 6 6, 1 2 4))"
        it "should convert a 4D Polygon to WKT" $ do
            toWKT (Polygon [LineString [Point {x=1, y=2, z=Just 3, m=Just 4}, Point {x=3, y=4, z=Just 5, m=Just 6}, Point {x=5, y=6, z=Just 7, m=Just 8}, Point {x=1, y=2, z=Just 3, m=Just 4}]] :: Polygon Int) 
                `shouldBe` "Polygon ZM ((1 2 3 4, 3 4 5 6, 5 6 7 8, 1 2 3 4))"
        it "should convert a 2D Polygon with a hole to WKT" $ do
            toWKT (Polygon [LineString [Point {x=1, y=2, z=Nothing, m=Nothing}, Point {x=3, y=4, z=Nothing, m=Nothing}, Point {x=5, y=6, z=Nothing, m=Nothing}, Point {x=1, y=2, z=Nothing, m=Nothing}], LineString [Point {x=2, y=3, z=Nothing, m=Nothing}, Point {x=4, y=5, z=Nothing, m=Nothing}, Point {x=6, y=7, z=Nothing, m=Nothing}, Point {x=2, y=3, z=Nothing, m=Nothing}]] :: Polygon Int) 
                `shouldBe` "Polygon ((1 2, 3 4, 5 6, 1 2), (2 3, 4 5, 6 7, 2 3))"

