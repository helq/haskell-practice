import           Foundation
import           Foundation.String.Read (readInteger)
import           System.Directory (listDirectory)
import qualified System.Directory as D (renameFile)
import           System.IO.Error (catchIOError, isDoesNotExistError, ioError)
import           Data.Foldable (forM_)
import qualified Text.Printf as P (printf)
import           Text.Megaparsec --(Parsec, Dec, noneOf, parseMaybe, char, many,
                                 -- some, count, string, try, digitChar, anyChar,
                                 -- optional)
import           Data.Text (Text, pack)

type FilePath = String

contents :: FilePath -> IO [FilePath]
contents dir = catchIOError (fmap fromList <$> listDirectory (toList dir)) $ \e ->
  if isDoesNotExistError e -- TODO: catch more errors
     then return []
     else ioError e

rename :: FilePath -> Maybe FilePath
rename = fmap fromList . parseMaybe changeNameParser . pack . toList
  where
    changeNameParser :: Parsec Dec Text [Char]
    changeNameParser = do
      beginning <- count 6 text_portion
      best_mark <- optional $ try (string "best" <* char_)
      rest_body <- many (try text_portion)
      num       <- fromMaybe 0 . readInteger . fromList <$> some digitChar
      let numStr = P.printf "%02d" num
      ext       <- char '.' *> many anyChar
      return $ intercalate "_" (beginning <> maybeToList best_mark <> [numStr] <> rest_body) <> "." <> ext

    text_portion :: Parsec Dec Text [Char]
    text_portion = many (noneOf ['_']) <* char_

    char_ :: Parsec Dec Text Char
    char_ = char '_'

basedir :: FilePath
basedir = "/tmp/fakefiles/"
--basedir = "/home/helq/Experimenting/lisi/sampleRNN_ICLR2017/results_3t_cond/DIMEX_3T/samples/"

main :: IO ()
main = do
  files <- contents basedir
  forM_ files $ \file ->
        case rename file of
          Just newFileName -> do
            putStrLn ("Renaming file from: " <> file <> " to: " <> newFileName)
            renameFile (basedir<>file) (basedir<>newFileName)
          Nothing -> putStrLn $ "No need to rename: " <> file


-- Converting functions from [Char] to String and viceversa
renameFile :: FilePath -> FilePath -> IO ()
renameFile = D.renameFile `on` toList
