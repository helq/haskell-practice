-- file: ch16/csv1.hs

module CSVParser (
  module CSVParser
) where

import Foundation
{-import Text.Parsec-}
import Data.Functor.Identity ()
import Text.Megaparsec
--import Text.Megaparsec.String
import Text.Megaparsec.Text
import Data.Text

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
--csvFile :: GenParser Char st [[String]] -- OLD Parsec library
--csvFile :: ParsecT Dec String Identity [[String]]
--csvFile :: Parsec Dec String [[String]]
csvFile :: Parser [[Text]]
csvFile =
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
--line :: GenParser Char st [String] -- OLD Parsec library
line :: Parser [Text]
line =
    do result <- cells
       {-eol :: Parser Text-}
       eol                       -- end of line
       return result

-- Build up a list of cells.  Try to parse the first cell, then figure out
-- what ends the cell.
--cells :: GenParser Char st [String] -- OLD Parsec library
cells :: Parser [Text]
cells =
    do first_ <- cellContent
       next <- remainingCells
       return (first_ : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
--remainingCells :: GenParser Char st [String] -- OLD Parsec library
remainingCells :: Parser [Text]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> return []                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
--cellContent :: GenParser Char st String -- OLD Parsec library
cellContent :: Parser Text
cellContent = fromList <$> many (noneOf [',','\n'])


-- The end of line character is \n
--eol :: GenParser Char st Char -- OLD Parsec library
{-
 -eol :: Parser Char
 -eol = char '\n'
 -}

--parseCSV :: String -> Either ParseError [[String]] -- OLD Parsec library
parseCSV :: Text -> Either (ParseError (Token Text) Dec) [[Text]]
parseCSV = parse csvFile "(unknown)"
