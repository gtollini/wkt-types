module Test.Data.Wkt.ToWKT (module Test.Data.Wkt.ToWKT) where
    
import Test.Hspec
-- import Test.QuickCheck
import Data.Wkt.Types
import Data.Wkt.Showables

points :: Spec
points = do
    describe "ToWKT" $ do
        it "should convert a 2D Point to WKT" $ do
            toWKT (Point {x=1, y=1, z=Nothing, m=Nothing}) `shouldBe` "Point (1 2)"
        it "should convert a 3D (Z) Point to WKT" $ do
            toWKT (Point {x=1, y=1, z=Just 1, m=Nothing}) `shouldBe` "Point Z (1 2 1)"
        it "should convert a 3D (M) Point to WKT" $ do
            toWKT (Point {x=1, y=1, z=Nothing, m=Just 1}) `shouldBe` "Point M (1 2 1)"
        it "should convert a 4D Point to WKT" $ do
            toWKT (Point {x=1, y=1, z=Just 1, m=Just 1}) `shouldBe` "Point ZM (1 2 1 1)"