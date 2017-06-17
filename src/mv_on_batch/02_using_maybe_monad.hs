import Foundation
import Foundation.Collection (getNonEmpty)
import Foundation.String.Read (readInteger)
import System.Directory (listDirectory)
import qualified System.Directory as D (renameFile)
import System.IO.Error (catchIOError, isDoesNotExistError, ioError)
import Data.Foldable (forM_)
import qualified System.FilePath as F (takeBaseName, replaceBaseName)
import qualified Text.Printf as P (printf)

type FilePath = String

contents :: FilePath -> IO [FilePath]
contents dir = catchIOError (fmap fromList <$> listDirectory (toList dir)) $ \e ->
  if isDoesNotExistError e -- TODO: catch more errors
     then return []
     else ioError e

rename :: FilePath -> Maybe FilePath
rename filePath = do
  parts    <- nonEmpty $ splitOn (=='_') basename
  num      <- readInteger (last parts)
  let numStr   = fromList (P.printf "%02d" num :: [Char])
  reodered <- reorder numStr . getNonEmpty $ parts
  return $ replaceBaseName filePath (unite reodered)
  where
    basename = takeBaseName filePath
    unite = mconcat . intersperse "_"

    reorder :: String -> [String] -> Maybe [String]
    reorder numStr parts = if length parts > 8
                              then Just $ start <> [numStr] <> end
                              else Nothing
      where (start, end') = splitAt splitNum parts
            (_, end)      = revSplitAt 1 end'
            splitNum = case last <$> nonEmpty (take 7 parts) of
                         Just "best" -> 7
                         _           -> 6

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
takeBaseName :: FilePath -> String
takeBaseName = fromList . F.takeBaseName . toList

replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName f s = fromList $ (F.replaceBaseName `on` toList) f s

renameFile :: FilePath -> FilePath -> IO ()
renameFile = D.renameFile `on` toList
