{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Wkt.MultiPoint (module Data.Wkt.MultiPoint) where

import Data.Wkt.Classes
import Data.Wkt.Point
import Data.Wkt.Helpers (generateZMString, showP, zmParser)
import Data.List (intercalate)
import Data.Text (pack, Text)
import Data.Attoparsec.Text (Parser, asciiCI, skipSpace, parseOnly)
import Control.Applicative ((<|>))

newtype MultiPoint a = MultiPoint [Point a]
    deriving (Functor, Eq)

instance Show a => Show (MultiPoint a) where
    show (MultiPoint points) = intercalate ", " (showP <$> points)

instance Show a => ToWKT (MultiPoint a) where
    toWKT multiPoint = "MultiPoint" <> zmString <> "(" <> pack (show multiPoint) <> ")"
        where
            (MultiPoint points) = multiPoint
            first = head points
            z' = z first
            m' = m first

            zmString = generateZMString z' m'

instance Valid (MultiPoint a) where
    isValid (MultiPoint points') = all isValid points'

instance FromWKT MultiPoint where
    fromWKT = either (error . show) id . parseOnly wktParser
instance ParseableFromWKT MultiPoint where
    wktParser = do
        skipSpace
        _ <- asciiCI "MULTIPOINT"
        (zFlag, mFlag) <- zmParser
        _ <- "("
        parseMultiPoint zFlag mFlag

parseMultiPoint :: Text -> Text -> Parser (MultiPoint Double)
parseMultiPoint zFlag mFlag = do
    MultiPoint <$> pointsParser zFlag mFlag
            where
                pointsParser zFlag' mFlag' = do
                    skipSpace
                    _ <- "("
                    newPoint <- parsePoint zFlag' mFlag'                    
                    closing <- "))" <|> ""
                    if closing /= "" then
                        return [newPoint]
                    else
                        (newPoint :) <$> (")" *> skipSpace *> "," *> pointsParser zFlag' mFlag')