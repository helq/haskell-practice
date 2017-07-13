-- file: ch16/csv1.hs

module CSVParser (
  module CSVParser
) where

import   Foundation
{-import   Text.Parsec-}
{-import   Data.Functor.Identity (Identity)-}
import   Text.Megaparsec
--import   Text.Megaparsec.String
import   Text.Megaparsec.Text
import   Data.Text (Text)

--csvFile :: ParsecT Dec Text Identity [[Text]]
csvFile :: Parser [[Text]]
csvFile = endBy line eol

line :: Parser [Text]
line = sepBy cell (char ',')

cell :: Parser Text
cell = fromList <$> many (noneOf [',','\n'])

parseCSV :: Text -> Either (ParseError (Token Text) Dec) [[Text]]
parseCSV = parse csvFile "(unknown)"
