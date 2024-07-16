module Test.Data.Wkt.FromWKT (module Test.Data.Wkt.FromWKT) where
    
import Test.Hspec

import Data.Wkt.Classes

import Test.Data.Wkt.TestElements
import Data.Wkt.Point (Point(..))

fromWktTests :: Spec
fromWktTests = do
    points
    -- lineStrings
    -- polygons
    -- multiPoints
    -- multiLineStrings
    -- multiPolygons
    -- geometryCollections

points :: Spec
points = do
    describe "FromWKT Points" $ do
        it "should convert a 2D Point to WKT" $ do
            floor <$> (fromWKT pointText :: Point Double)  `shouldBe` point
        it "should convert a 3D (Z) Point to WKT" $ do
            floor <$> (fromWKT pointZText :: Point Double)  `shouldBe` pointZ
        it "should convert a 3D (M) Point to WKT" $ do
            floor <$> (fromWKT pointMText :: Point Double)  `shouldBe` pointM
        it "should convert a 4D Point to WKT" $ do
            floor <$> (fromWKT pointZMText :: Point Double) `shouldBe` pointZM