{-# LANGUAGE LambdaCase #-}

import           Control.Monad     (forM_)
import           Foundation
import           Text.Megaparsec
                                 -- some, count, string, try, digitChar, anyChar,
                                 -- optional)
import           Data.Text         (Text, lines, unpack)
import           Data.Time
import           Lens.Micro        (Lens')
--import           Lens.Micro.TH (makeLenses)
import qualified GHC.Num           as Num (negate)
import           Lens.Micro.Extras (view)
--import qualified GHC.Real as Real (fromRational)
import qualified Data.Text.IO      as Text (readFile)
import           GHC.Float         (fromRat)
import           GHC.Real          (toRational)
import           Prelude           (logBase)

-- workaround in the meantime, the people from foundation are still debating what to do with show
-- for more info: https://github.com/haskell-foundation/foundation/issues/193
import qualified Prelude           (show)

{- Structures definitions -}

data Priority   = NotUrgent | Urgent    deriving (Show, Eq)
data Importance = Low | Medium | High   deriving (Show, Eq)

data NextAction = NextAction
  {
    _priority    :: Priority
  , _importance  :: Importance
  --, _age          :: NominalDiffTime -- this is dummy, it can be accessed by a lens called `age`
  , _createdDate :: UTCTime
  , _message     :: Text
  } deriving (Eq)

--makeLenses ''NextAction

ageTo :: UTCTime -> Lens' NextAction NominalDiffTime
ageTo now = ageTo'
  where
    ageTo' :: Lens' NextAction NominalDiffTime
    ageTo' elt_fn na = fmap (\anAge-> na {_createdDate = addUTCTime (Num.negate anAge) now}) (elt_fn $ diffUTCTime now (_createdDate na))

--instance Ord NextAction where
--  compare = compare `on` view createdDate

instance Show NextAction where
  show (NextAction p i d msg) = "- {" <> (prio:'|':imp:'|': date) <> "} " <> (fromList . unpack $ msg)
    where prio = case p of Urgent -> 'U'; NotUrgent -> 'N'
          imp  = case i of Low -> 'L'; Medium -> 'M'; High -> 'H'
          date = fmap (\case '-'->'.'; c->c) -- replacing all '-' for '.'
                  . take 10 -- ugly, find a more standard way, using UTCTime helper functions
                  . toList $ show d :: [Char]

{- Parser -}
-- TODO: read more than oneline tasks, tasks may be sometimes multilined
lineParser :: Parsec Dec Text NextAction
lineParser = do
  _ <- optional space >> char '-' >> space >> char '{'
  priority_   <- priorityParser   <* char '|'
  importance_ <- importanceParser <* char '|'
  date        <- dateParser       <* (char '}' >> space)
  msg         <- fromList <$> many anyChar
  return $ NextAction priority_ importance_ date msg

priorityParser :: Parsec Dec Text Priority
priorityParser = (NotUrgent <$ char 'N')
             <|> (Urgent    <$ char 'U')

importanceParser :: Parsec Dec Text Importance
importanceParser = (Low    <$ char 'L')
               <|> (Medium <$ char 'M')
               <|> (High   <$ char 'H')

dateParser :: Parsec Dec Text UTCTime
dateParser = do
  date <- year <> dot <> mnday <> dot <> mnday
  parseTimeM True defaultTimeLocale "%Y.%m.%d" date
    where
      dot   = (:[]) <$> char '.'
      year  = count 4 . oneOf $ ['0'..'9']
      mnday = count 2 . oneOf $ ['0'..'9']

extractTasks :: Text -> [NextAction]
extractTasks = catMaybes . fmap parseLine . lines
  where
    parseLine = parseMaybe lineParser

{- Sorting function -}

myOrderValueWith :: UTCTime -> NextAction -> Double
myOrderValueWith now task = extractValue task
  where extractValue (NextAction p i _ _) = negate $ g p * h i * logBase 1.5 (age/secsADay+3)
        g NotUrgent = 1
        g Urgent    = 2
        h Low    = 1
        h Medium = 1.5
        h High   = 2
        age = fromRat . toRational . view (ageTo now) $ task
        secsADay = 24*60*60 -- seconds in a day

{- Main program -}

filename :: [Char]
filename = "/home/helq/planning/actionable/_next_actions.md"

getTasks :: IO [NextAction]
getTasks = do
  -- TODO: take into account possible exceptions raised on runtime
  file <- Text.readFile filename
  --putStrLn . fromList . unpack $ file
  return $ extractTasks file

main :: IO ()
main = do
  now <- getCurrentTime -- TODO: get current day from it not, current hour
  --let todayAction = NextAction NotUrgent Low now "Not much, really"
  --    aDay = Real.fromRational (60*60*24) :: NominalDiffTime
  --    yesterdayAction = set (ageTo now) aDay todayAction
  --putStrLn . show $ yesterdayAction
  tasks <- getTasks
  let tasksOrdered = sortBy (compare `on` myOrderValueWith now) tasks
  forM_ tasksOrdered $ \task-> do
    putStr $ show (myOrderValueWith now task) <> " " -- priority number
    putStrLn . show $ task
    {-putStrLn . show . fromRat . toRational . view (ageTo now) $ task-}
