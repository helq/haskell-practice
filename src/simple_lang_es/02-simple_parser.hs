{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative   (empty, optional)
import           Data.Foldable         (fold, foldMap)
import qualified Data.List.NonEmpty    as NE (last)
import           Data.Text             (Text)
import           Data.Text.IO          (getContents)
import           Foundation
import           Text.Megaparsec       (Dec, Parsec, alphaNumChar, between,
                                        char, endBy, eof, eol, letterChar, many,
                                        manyTill, oneOf, parse, sepBy, sepBy1,
                                        sepEndBy, skipMany, skipSome, spaceChar,
                                        string, try, unPos, (<?>))
import           Text.Megaparsec.Error (ErrorItem (..), ParseError (..))
import           Text.Megaparsec.Expr  (Operator (InfixL, InfixR, Prefix),
                                        makeExprParser)
import           Text.Megaparsec.Lexer (charLiteral, decimal, float,
                                        skipLineComment)
import qualified Text.Megaparsec.Lexer as ML
import           Text.Megaparsec.Pos   (SourcePos (..))
--import           Text.Megaparsec

{-
 - To check for test cases:
 for i in src/simple_lang_es/test_cases/parser/*/in*.txt; do
   echo -n "$i: "
   stack exec -- simple_parser < "$i"
 done
 -}

data SimpleProg = SimpleProg [Function] [Sentence]
  deriving (Show)

data Function = Function Id [Id] [Sentence] -- id args
  deriving (Show)

data Sentence =
      Assign Id Expr
    | SExpr Expr
    | IfElse Expr [Sentence] [Sentence]
    | ForLoop Id Expr [Sentence]
    | WhileLoop Expr [Sentence]
    | Importar [Id] Id -- import _ from _
    | Retorno Expr -- (special meaning, valid only inside functions)
    | Leer Id
  deriving (Show)

data Id =
      Id String
    | IdComp [String]
    | IdSubscr [String] [Expr]
  deriving (Show)

data Expr =
      EId Id
    | EVal Val
    | UniOp Op Expr
    | BinOp Op Expr Expr
    | FunAp Id [Expr]
    | EList [Expr]
    | EDict [(Expr, Expr)]
  deriving (Show)

data Val =
    VBool Bool
  | VInt Integer
  | VDouble Double
  | VString String
  | VList [Val]
  | VDict [(Val, Val)]
  | VNil
  deriving (Show)

data Op = ONeq -- !
        | OPow -- ^
        | OMul -- *
        | ODiv -- /
        | OMod -- %
        | OAdd -- +
        | OSub -- -
        | OLE  -- <=
        | OBE  -- >=
        | OLT  -- <
        | OBT  -- >
        | OAnd -- &&
        | OOr  -- ||
        | OEQ  -- ==
        | ONE  -- !=
  deriving (Show)

-- TODO: consider optionally to not consume a eol character before finishing the file
progParser :: Parsec Dec Text SimpleProg
progParser = manySpaceLines >>
             (SimpleProg
             <$> (function `endBy` someSpaceLines)
             <*> (try (sentence []) `sepEndBy` someSpaceLines)
             ) <* eof

function :: Parsec Dec Text Function
function = do
  _ <- symbol "funcion"
  id' <- idToken
  args <- parens $ idToken `sepBy` comma
  _ <- someSpaceLines
  stns <- try (sentence [retorno]) `endBy` someSpaceLines
  _ <- symbol "end" >> symbol "funcion"
  pure $ Function id' args stns -- stns

retorno :: Parsec Dec Text Sentence
retorno = symbol "retorno" >> (Retorno <$> parens (try expr <|> pure (EVal VNil)))

-- TODO: THINGS to finish:
-- * all operations
sentence :: [Parsec Dec Text Sentence] -> Parsec Dec Text Sentence
sentence additional = try assign
                  <|> try ifelse
                  <|> try if_
                  <|> try whileloop
                  <|> try forloop
                  <|> try importar
                  <|> try leer
                  <|> foldr ((<|>) . try) (fail "Expecting sentence, but got nothing") additional
                  <|> sexpr
  where
    assign = Assign <$> (idTokenComp <* symbol "=") <*> expr
    sexpr = SExpr <$> expr
    if_ = do
      _ <- symbol "if"
      e1 <- parens expr
      _ <- manySpaceLines
      true_branch <- try bracketed_sentences <|> single_sentence
      pure $ IfElse e1 true_branch []

    ifelse = do
      IfElse e1 true_branch _ <- if_
      _ <- manySpaceLines
      false_branch <- do
          _ <- symbol "else"
          _ <- manySpaceLines
          bracketed_sentences <|> single_sentence
      pure $ IfElse e1 true_branch false_branch

    whileloop = do
      _ <- symbol "while"
      e1 <- parens expr
      _ <- manySpaceLines
      snts <- try bracketed_sentences <|> single_sentence
      pure $ WhileLoop e1 snts

    forloop = do
      _ <- symbol "for"
      id_ <- idToken
      _ <- symbol "in"
      iterable <- expr
      _ <- manySpaceLines
      snts <- try bracketed_sentences <|> single_sentence
      pure $ ForLoop id_ iterable snts

    importar = (symbol "importar" >> (Importar [] <$> idTokenComp))
            <|> (symbol "importar" >> (Importar <$> (idToken `sepBy` comma) <*> (symbol "desde" >> idTokenComp)))

    leer = symbol "leer" >> Leer <$> parens idTokenComp

    bracketed_sentences = brackets (manySpaceLines >> sentence additional `sepEndBy` someSpaceLines)
    single_sentence = (:[]) <$> sentence additional

funap :: Parsec Dec Text Expr
funap = FunAp <$> idTokenComp <*> parens (expr `sepBy` comma) <?> "function application"

expr :: Parsec Dec Text Expr
expr = makeExprParser term table <?> "expression"

term :: Parsec Dec Text Expr
term =   parens expr
     <|> try funap
     <|> (EVal <$> val)
     <|> (EId <$> idTokenComp)
     <|> (EList <$> angleBrackets (expr `sepBy` comma))
     <|> (EDict <$> brackets (dict `sepBy` comma))
     <?> "term"
    where
      dict = (,) <$> expr <*> (symbol ":" >> expr)

table :: [[Operator (Parsec Dec Text) Expr]]
table = [ [ prefix  "!"  (UniOp ONeq) ]
        , [ binaryR "^"  (BinOp OPow) ]
        , [ binary  "*"  (BinOp OMul), binary  "/"  (BinOp ODiv)
          , binary  "%"  (BinOp OMod) ]
        , [ binary  "+"  (BinOp OAdd), binary  "-"  (BinOp OSub)  ]
        , [ binary  "<=" (BinOp OLE),  binary  ">=" (BinOp OBE)
          , binary  "<"  (BinOp OLT),  binary  ">"  (BinOp OBT) ]
        , [ binary  "&&" (BinOp OAnd) ]
        , [ binary  "||" (BinOp OOr) ]
        , [ binary  "==" (BinOp OEQ),  binary  "!=" (BinOp ONE) ] ]
  where
    binary  name f = InfixL  (f <$ symbol name)
    binaryR name f = InfixR  (f <$ symbol name)
    prefix  name f = Prefix  (f <$ symbol name)
    --postfix name f = Postfix (f <$ symbol name)

val :: Parsec Dec Text Val
val = bool <|> try float' <|> nil <|> int <|> string_
  where
    symToVal str val' = const val' <$> lexeme (string str)
    bool = symToVal "true" (VBool True)
       <|> symToVal "false" (VBool False)
    int     = VInt    <$> ML.signed spaces (lexeme decimal)
    nil     = VNil    <$  symbol "nil"
    float'  = VDouble <$> ML.signed spaces (lexeme float)
    string_ = VString . fromList <$> lexeme (char '"' >> manyTill charLiteral (char '"'))

lexeme :: Parsec Dec Text a -> Parsec Dec Text a
lexeme = ML.lexeme spaces

symbol :: [Char] -> Parsec Dec Text [Char]
symbol = ML.symbol spaces

parens :: Parsec Dec Text a -> Parsec Dec Text a
parens = between (symbol "(") (symbol ")")

brackets :: Parsec Dec Text a -> Parsec Dec Text a
brackets = between (symbol "{") (symbol "}")

angleBrackets :: Parsec Dec Text a -> Parsec Dec Text a
angleBrackets = between (symbol "[") (symbol "]")

comma :: Parsec Dec Text [Char]
comma = symbol ","

spaces :: Parsec Dec Text ()
spaces = skipMany (oneOf [' ', '\t'])

manySpaceLines :: Parsec Dec Text ()
manySpaceLines = ML.space (skipSome spaceChar) (skipLineComment "#") empty

someSpaceLines :: Parsec Dec Text ()
someSpaceLines = onlySpacesLeftBefore >> eol >> manySpaceLines
  where
    onlySpacesLeftBefore = try (spaces >> skipLineComment "#") <|> spaces

-- This one fails with input " \t # ", even though skipLineComment should consume #
--spaces_ :: Parsec Dec Text ()
--spaces_ = ML.space spaces (skipLineComment "#") empty

-- Note: "log" and "leer" are normal functions, not primitives at the syntactic level
reservedwords :: [[Char]]
reservedwords = ["while", "for", "if", "funcion", "false", "true", "importar",
                "in", "retorno", "end", "nil", "desde", "todo", "else", "leer"]

idTokenRaw :: Parsec Dec Text String
idTokenRaw = (lexeme . try $ (fromList <$> (identifier >>= check))) <?> "id name"
  where
    identifier = fmap (:[]) (letterChar <|> char '_') <> many (alphaNumChar <|> char '_')
    check x = if x `elem` reservedwords
                 then fail . fromList $ "keyword `" <> x <> "` cannot be an identifier"
                 else pure x

idToken :: Parsec Dec Text Id
idToken = Id <$> idTokenRaw

idTokenComp :: Parsec Dec Text Id
idTokenComp = do
  id_ <- idTokenRaw `sepBy1` symbol "."
  subscr <- optional . angleBrackets $ expr `sepBy` comma
  pure $ case subscr of
           Nothing   -> IdComp id_
           Just subs -> IdSubscr id_ subs

main :: IO ()
main = do
  str <- getContents
  {-putStrLn str-}
  putStrLn $ case parse progParser "stdin" str of
               {-Left  e  -> show e-}
               {-Right ls -> show ls-}
               Left  e -> let line = show . unPos . sourceLine   . NE.last $ errorPos e
                              col  = show . unPos . sourceColumn . NE.last $ errorPos e
                              erroItemToString :: ErrorItem Char -> String
                              erroItemToString = \case
                                                    Tokens ts  -> fromList . toList $ ts
                                                    Label  l   -> fromList . toList $ l
                                                    EndOfInput -> "end of input"
                              found = foldMap erroItemToString . errorUnexpected $ e
                              expected = fold . intersperse "','" . fmap erroItemToString . toList . errorExpected $ e
                           in "<"<>line<>":"<>col<>"> Error sintáctico. Encontrado: '"<>found<>"'; se esperaba: '"<>expected<>"'."
               Right _ -> "El análisis sintáctico ha finalizado correctamente." --show ls
