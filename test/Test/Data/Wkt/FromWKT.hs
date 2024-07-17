module Test.Data.Wkt.FromWKT (module Test.Data.Wkt.FromWKT) where
    
import Test.Hspec

import Data.Wkt.Classes

import Test.Data.Wkt.TestElements
import Data.Wkt.Point (Point(..))
import Data.Wkt.LineString (LineString(..))
import Data.Wkt.Polygon (Polygon(..))
import Data.Wkt.MultiPoint (MultiPoint(..))
import Data.Wkt.MultiLineString (MultiLineString(..))


fromWktTests :: Spec
fromWktTests = do
    points
    lineStrings
    polygons
    multiPoints
    multiLineStrings
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
        
polygons :: Spec
polygons = do
    describe "FromWKT Polygons" $ do
        it "should convert a WKT Text to a 2D Polygon" $ do
            floor <$> (fromWKT polygonText :: Polygon Double)  `shouldBe` polygon
        it "should convert a WKT Text to a 3D (Z) Polygon " $ do
            floor <$> (fromWKT polygonZText :: Polygon Double)  `shouldBe` polygonZ
        it "should convert a WKT Text to a 3D (M) Polygon" $ do
            floor <$> (fromWKT polygonMText :: Polygon Double)  `shouldBe` polygonM
        it "should convert a WKT Text to a 4D Polygon" $ do
            floor <$> (fromWKT polygonZMText :: Polygon Double) `shouldBe` polygonZM
        it "should convert a WKT Text to Polygon with two rings" $ do
            floor <$> (fromWKT polygonRText :: Polygon Double) `shouldBe` polygonR

multiPoints :: Spec
multiPoints = do
    describe "FromWKT MultiPoints" $ do
        it "should convert a WKT Text to a 2D MultiPoint" $ do
            floor <$> (fromWKT multiPointText :: MultiPoint Double)  `shouldBe` multiPoint
        it "should convert a WKT Text to a 3D (Z) MultiPoint " $ do
            floor <$> (fromWKT multiPointZText :: MultiPoint Double)  `shouldBe` multiPointZ
        it "should convert a WKT Text to a 3D (M) MultiPoint" $ do
            floor <$> (fromWKT multiPointMText :: MultiPoint Double)  `shouldBe` multiPointM
        it "should convert a WKT Text to a 4D MultiPoint" $ do
            floor <$> (fromWKT multiPointZMText :: MultiPoint Double) `shouldBe` multiPointZM

multiLineStrings :: Spec
multiLineStrings = do
    describe "FromWKT MultiLineStrings" $ do
        it "should convert a WKT Text to a 2D MultiLineString" $ do
            floor <$> (fromWKT multiLineStringText :: MultiLineString Double)  `shouldBe` multiLineString
        it "should convert a WKT Text to a 3D (Z) MultiLineString " $ do
            floor <$> (fromWKT multiLineStringZText :: MultiLineString Double)  `shouldBe` multiLineStringZ
        it "should convert a WKT Text to a 3D (M) MultiLineString" $ do
            floor <$> (fromWKT multiLineStringMText :: MultiLineString Double)  `shouldBe` multiLineStringM
        it "should convert a WKT Text to a 4D MultiLineString" $ do
            floor <$> (fromWKT multiLineStringZText :: MultiLineString Double) `shouldBe` multiLineStringZ