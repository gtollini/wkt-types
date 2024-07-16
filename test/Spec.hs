module Main where

import Test.Hspec
import Test.Data.Wkt.ToWKT (toWktTests)
import Test.Data.Wkt.FromWKT (fromWktTests)

main :: IO ()
main = hspec $ do
    toWktTests
    fromWktTests