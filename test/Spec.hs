module Main where

import Test.Hspec
import Test.Data.Wkt.ToWKT

main :: IO ()
main = hspec $ do
    points
    lineStrings
    polygons
    multiPoints
    multiLineStrings
    multiPolygons
