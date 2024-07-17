{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Wkt.LineString (module Data.Wkt.LineString) where

import Data.Wkt.Classes
import Data.Wkt.Point (Point(..), parsePoint, pointDimension)
import Data.List (intercalate)
import Data.Wkt.Helpers (generateZMString, zmParser)
import Data.Text (pack, Text)
import Data.Attoparsec.Text
    ( asciiCI,
      skipSpace,
      parseOnly, Parser )
import Control.Applicative ((<|>))

newtype LineString a = LineString [Point a]
    deriving (Eq, Functor)
instance Show a => Show (LineString a) where
    show (LineString line) = intercalate ", " (show <$> line)

instance Show a => ToWKT (LineString a) where
    toWKT lineString
        | null line = "EMPTY"
        | otherwise = "LineString" <> zmString <> "(" <> pack (show lineString) <> ")"
        where
            LineString line = lineString
            first = head line
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Valid (LineString a) where
    isValid (LineString []) = True
    isValid (LineString (fpoint:tpoints)) = all (==fpointDimension) tpointDimensions
        where
            fpointDimension = pointDimension fpoint
            tpointDimensions = pointDimension <$> tpoints

instance FromWKT LineString where
    fromWKT = either (error . show) id . parseOnly wktParser
instance ParseableFromWKT LineString where
    wktParser = do
        skipSpace
        _ <- asciiCI "LINESTRING"
        (zFlag, mFlag) <- zmParser
        _ <- "("
        parseLineString zFlag mFlag

parseLineString :: Text -> Text -> Parser (LineString Double)
parseLineString zFlag mFlag = do
    LineString <$> pointsParser zFlag mFlag
            where
                pointsParser zFlag' mFlag' = do
                    newPoint <- parsePoint zFlag' mFlag'                    
                    closing <- ")" <|> ""
                    if closing /= "" then
                        return [newPoint]
                    else
                        (newPoint :) <$> ("," *> pointsParser zFlag' mFlag')