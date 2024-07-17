module Test.Data.Wkt.FromWKT (module Test.Data.Wkt.FromWKT) where
    
import Test.Hspec

import Data.Wkt.Classes

import Test.Data.Wkt.TestElements
import Data.Wkt.Point (Point(..))
import Data.Wkt.LineString (LineString(..))
-- import qualified Data.Wkt.Point as P
-- import qualified Data.Wkt.LineString as LS


fromWktTests :: Spec
fromWktTests = do
    points
    lineStrings
    -- polygons
    -- multiPoints
    -- multiLineStrings
    -- multiPolygons
    -- geometryCollections

points :: Spec
points = do
    describe "FromWKT Points" $ do
        it "should convert a WKT Text to a 2D Point" $ do
            floor <$> (fromWKT pointText :: Point Double)  `shouldBe` point
        it "should convert a WKT Text to a 3D (Z) Point " $ do
            floor <$> (fromWKT pointZText :: Point Double)  `shouldBe` pointZ
        it "should convert a WKT Text to a 3D (M) Point" $ do
            floor <$> (fromWKT pointMText :: Point Double)  `shouldBe` pointM
        it "should convert a WKT Text to a 4D Point" $ do
            floor <$> (fromWKT pointZMText :: Point Double) `shouldBe` pointZM

lineStrings :: Spec
lineStrings = do
    describe "FromWKT LineStrings" $ do
        it "should convert a WKT Text to a 2D LineString" $ do
            floor <$> (fromWKT lineStringText :: LineString Double)  `shouldBe` lineString
        it "should convert a WKT Text to a 3D (Z) LineString " $ do
            floor <$> (fromWKT lineStringZText :: LineString Double)  `shouldBe` lineStringZ
        it "should convert a WKT Text to a 3D (M) LineString" $ do
            floor <$> (fromWKT lineStringMText :: LineString Double)  `shouldBe` lineStringM
        it "should convert a WKT Text to a 4D LineString" $ do
            floor <$> (fromWKT lineStringZMText :: LineString Double) `shouldBe` lineStringZM
        