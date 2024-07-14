module Data.Wkt.Classes (module Data.Wkt.Classes) where

class Valid a where
    isValid :: a -> Bool

class ToWKT a where
    toWKT :: a -> String