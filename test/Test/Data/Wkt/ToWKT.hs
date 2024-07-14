module Test.Data.Wkt.ToWKT (module Test.Data.Wkt.ToWKT) where
    
import Test.Hspec
-- import Test.QuickCheck
import Data.Wkt.Types
import Data.Wkt.Showables

points :: Spec
points = do
    describe "ToWKT" $ do
        it "should convert a 2D Point to WKT" $ do
            toWKT (Point {x=1, y=2, z=Nothing, m=Nothing}) `shouldBe` "Point (1 2)"
        it "should convert a 3D (Z) Point to WKT" $ do
            toWKT (Point {x=1, y=2, z=Just 3, m=Nothing}) `shouldBe` "Point Z (1 2 3)"
        it "should convert a 3D (M) Point to WKT" $ do
            toWKT (Point {x=1, y=2, z=Nothing, m=Just 4}) `shouldBe` "Point M (1 2 4)"
        it "should convert a 4D Point to WKT" $ do
            toWKT (Point {x=1, y=2, z=Just 3, m=Just 4}) `shouldBe` "Point ZM (1 2 3 4)"