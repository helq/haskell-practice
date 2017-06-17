import Foundation
import Foundation.Collection (getNonEmpty)
import Foundation.String.Read (readInteger)
import System.Directory (listDirectory)
import qualified System.Directory as D (renameFile)
import System.IO.Error (catchIOError, isDoesNotExistError, ioError)
import Data.Foldable (forM_)
import qualified System.FilePath as F (takeBaseName, replaceBaseName)

type FilePath = String

contents :: FilePath -> IO [FilePath]
contents dir = catchIOError (fmap fromList <$> listDirectory (toList dir)) $ \e ->
  if isDoesNotExistError e -- TODO: catch more errors
     then return []
     else ioError e

rename :: FilePath -> FilePath
rename filePath = case nonEmpty parts of
                    Nothing     -> filePath
                    Just parts' -> replaceBaseName filePath $ unite (reorder parts')
  where
    basename = takeBaseName filePath
    parts = splitOn (=='_') basename
    isNumber str = isJust (readInteger str)
    unite = mconcat . intersperse "_" . getNonEmpty

    reorder :: NonEmpty [String] -> NonEmpty [String]
    reorder parts' = if isNumber (last parts')
                        then fmapToNonEmpty reorder' parts'
                        else parts'
        where
          fmapToNonEmpty f = fromMaybe parts' . nonEmpty . f . getNonEmpty
          lenParts = length parts'

    reorder' :: [String] -> [String]
    reorder' parts = start <> num <> end
      where (start, end') = splitAt splitNum parts
            (num, end)    = revSplitAt 1 end'
            splitNum = case last <$> nonEmpty (take 7 parts) of
                         Just "best" -> 7
                         _           -> 6

basedir = "/tmp/fakefiles/"
--basedir = "/home/helq/Experimenting/lisi/sampleRNN_ICLR2017/results_3t_cond/DIMEX_3T/samples/"

main :: IO ()
main = do
  files <- contents basedir
  forM_ files $ \file -> do
    let newFileName = rename file
    putStrLn ("Renaming file from: " <> file <> " to: " <> newFileName)
    {-renameFile (basedir<>file) (basedir<>newFileName)-}


-- Converting functions from [Char] to String and viceversa
takeBaseName :: FilePath -> String
takeBaseName = fromList . F.takeBaseName . toList

replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName f s = fromList $ (F.replaceBaseName `on` toList) f s

renameFile :: FilePath -> FilePath -> IO ()
renameFile = D.renameFile `on` toList
