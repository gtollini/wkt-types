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