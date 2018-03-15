{-
 - This is basically overkill for a simple thing like "lexing". Megaparsec +
 - Haskell is much, much more powerful than a lexer. The simple rule of
 - progLexer could be extended to parse using a particular grammar and create
 - an AST on the fly.
 -}

import           Control.Applicative   (empty)
import           Data.List             (elemIndex)
import           Data.Text             (Text)
import           Data.Text.IO          (getContents)
import           Foundation
import           Text.Megaparsec       (Dec, ParseError (..), Parsec, Pos,
                                        SourcePos (..), alphaNumChar, char,
                                        getPosition, letterChar, many, manyTill,
                                        noneOf, parse, sepEndBy, skipSome,
                                        sourceLine, spaceChar, string, try,
                                        unPos)
import           Text.Megaparsec.Char  (newline)
import           Text.Megaparsec.Lexer (charLiteral, decimal, float,
                                        skipLineComment)
import qualified Text.Megaparsec.Lexer as ML (space)
{-import           Foundation.String.Read-}
{-import qualified Text.Printf as P (printf)-}

{-
 to prove that the lexer works according to the specifications run:
 $ for i in src/simple_lexer/testCases/*/in*.txt; do
       echo "checking 'lexing' of: $i"
       diff --ignore-all-space $(echo "$i" | sed -e "s/in/out/") <(stack exec simple_lexer < "$i")
   done
-}

-- simulating a lexer by parsing the whole text as a list of tokens (for this
-- all that it needs to be used is `sepEndBy`)
progLexer :: Parsec Dec Text [[Char]]
progLexer = spaces >> tokens' `sepEndBy` spaces
  where
    -- this parser captures anything except an empty character
    tokens' =  idToken
           <|> try floatToken
           <|> intToken
           <|> foldl' (\x y->x <|> uncurry opToken y) empty ops
           <|> stringLiteral
           <|> notAnythingToken

spaces :: Parsec Dec Text ()
spaces = ML.space (skipSome (spaceChar <|> newline))
                  (skipLineComment "#")
                  empty
                  {-(skipBlockComment "/*" "*/")-}

reservedwords :: [[Char]]
reservedwords = ["while", "for", "if", "log", "funcion", "false", "true",
                 "importar", "in", "retorno", "end", "nil", "desde",
                 "todo", "else"]

idToken :: Parsec Dec Text [Char]
idToken = do
  pos <- currPos
  name <- fmap (:[]) letterChar <> many alphaNumChar
  case name `elemIndex` reservedwords of
    Just _  -> return ("<"<>name<>","<>showPos pos<>">")
    Nothing -> return ("<id,"<>name<>","<>showPos pos<>">")

floatToken :: Parsec Dec Text [Char]
floatToken = do
  pos <- currPos
  d <- float
  return ("<token_float,"<>toList (show d)<>","<>showPos pos<>">")

intToken :: Parsec Dec Text [Char]
intToken = do
  pos <- currPos
  d <- decimal
  return ("<token_integer,"<>toList (show d)<>","<>showPos pos<>">")

opToken :: [Char] -> [Char] -> Parsec Dec Text [Char]
opToken str tokname = do
  pos <- currPos
  _ <- string str
  return ("<"<>tokname<>","<>showPos pos<>">")

stringLiteral :: Parsec Dec Text [Char]
stringLiteral = do
  pos <- currPos
  _ <- char '"'
  str <- manyTill charLiteral (char '"')
  return ("<token_string,"<>str<>","<>showPos pos<>">")

ops :: [([Char], [Char])]
ops = [(">=", "token_mayor_igual"), ("<=", "token_menor_igual"),
       ("==", "token_igual_num"),   ("!=", "token_diff_num"),
       ("&&", "token_and"),         ("||", "token_or"),
       ("{", "token_llave_izq"), ("}", "token_llave_der"), ("[", "token_cor_izq"),
       ("]", "token_cor_der"),   ("(", "token_par_izq"),   (")", "token_par_der"),
       (">", "token_mayor"),     ("<", "token_menor"),     ("!", "token_not"),
       ("+", "token_mas"),       ("-", "token_menos"),     ("*", "token_mul"),
       ("/", "token_div"),       ("%", "token_mod"),       ("^", "token_pot"),
       ("=", "token_assign"),    (",", "token_coma"),      (".", "token_point"),
       (":", "token_dosp")]

showPos :: (Pos, Pos) -> [Char]
showPos (l,c) = posToText l <> "," <> posToText c

currPos :: Parsec Dec Text (Pos, Pos)
currPos = do
  SourcePos{sourceLine = line, sourceColumn = col} <- getPosition
  return (line, col)

-- This is ugly, there is no need to do this. In a real case this token should
-- never be used but it is used to simulate a lexer which reads character by
-- character and outputs to the console.
notAnythingToken :: Parsec Dec Text [Char]
notAnythingToken = do
  (line,col) <- currPos
  -- the lexicon doesn't accepts anything else, all tokens have been captured
  -- with the rules above. This rule just captures whatever the parser hasn't
  -- yet and tells the user there has been a problem with the lexer.
  _ <- noneOf [' ', '\n']
  return (">>> Error lexico(linea:"<>posToText line<>",posicion:"<>posToText col<>")")

lexerSimpleLang :: Text -> Either (ParseError Char Dec) [String]
lexerSimpleLang = fmap (fmap fromList) . parse progLexer "afile.txt"

main :: IO ()
main = do
  str <- getContents
  {-putStrLn str-}
  putStrLn $ case lexerSimpleLang str of
               Left  e  -> show e
               Right ls -> intercalate "\n" $ takeWhileOneMore ((/= ">>>") . take 3) ls

-- additional tooling functions
takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

posToText :: Pos -> [Char]
posToText = toList . show . unPos
