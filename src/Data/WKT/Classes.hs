module Data.WKT.Classes (module Data.WKT.Classes) where
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (Text)


class Valid a where
    isValid :: a -> Bool

class ToWKT a where
    toWKT :: a -> Text

class ParseableFromWKT a => FromWKT a where
    fromWKT :: Text -> a Double
    fromWKT = either (error . show) id . parseOnly wktParser
class ParseableFromWKT a where
    wktParser :: Parser (a Double)

