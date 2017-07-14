{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import           Foundation
import           Text.Megaparsec
import qualified Text.Megaparsec as T
import           Data.Text

{-# ANN module ("HLint: ignore Use String" :: Text) #-}

toChars :: Stream s => s -> [Token s]
toChars = maybe [] (\(ts,ss')->ts:toChars ss') . T.uncons

main :: IO ()
main =
  putStrLn .
    (show :: [[Char]]->String) .
    fmap (:[]) $
    toChars ("Hey :D"::Text)
    --toChars ("Hey :D"::[Char])
    --(toChars::Text->[Char]) "Hey :D"
    --toChars "Hey :D" -- doesn't work
    --(toChars "Hey :D" :: (s ~ Text, IsString s, Stream s) => [Token s]) -- also doesn't work

{- For more info on why isn't GHC able to infer the types correctly look at
 - https://stackoverflow.com/questions/45069784/cannot-match-type-of-expression-using-typefamilies
-}
