module Data.Wkt.Classes (module Data.Wkt.Classes) where
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (Text)


class Valid a where
    isValid :: a -> Bool

class ToWKT a where
    toWKT :: a -> Text

class ParseableFromWKT a => FromWKT a where
    fromWKT :: Text -> a
    fromWKT = either (error . show) id . parseOnly wktParser
class ParseableFromWKT a where
    wktParser :: Parser a

