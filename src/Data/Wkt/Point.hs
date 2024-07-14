{-# LANGUAGE NamedFieldPuns #-}
module Data.Wkt.Point (module Data.Wkt.Point) where
import Data.Wkt.Classes
import Data.Maybe (isJust)

data Point a = Point{
    x :: a,
    y :: a,
    z :: Maybe a,
    m :: Maybe a
}

    deriving (Eq, Ord)
instance Show a => Show (Point a) where
    show (Point {x,y,z,m}) = pointValue
        where
            -- TODO: check if it's okay to have . or if I must always e.
            x' = show x
            y' = show y
            z' = maybe "" ((" " <>). show) z
            m' = maybe "" ((" " <>). show) m
            pointValue = x' <> " " <> y' <> z' <> m'

instance Show a => ToWKT (Point a) where
    toWKT point = "Point" <> zmString <> "(" <> show point <> ")"
        where
            Point{z,m} = point
            zmString
                |isJust z && isJust m = " ZM "
                |isJust z = " Z "
                |isJust m = " M "
                |otherwise = " "

-- Just here for completeness.
instance Valid (Point a) where
    isValid (Point {}) = True