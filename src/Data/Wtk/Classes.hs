module Data.Wtk.Classes (Valid(..)) where

class Valid a where
    isValid :: a -> Bool