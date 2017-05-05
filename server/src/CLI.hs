{-# LANGUAGE DataKinds #-}
module Main where

import           Common
import qualified DB as DB
import           Types

import qualified OldData

import           Control.Lens          (over)
import           Control.Lens.Monadic  (overM)
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import qualified Data.TCache             as T
import qualified Data.TCache.ID          as ID
import           Data.Text (pack)
import           Data.Tree (Tree(Node))
import qualified System.Console.Argument as CP
import qualified System.Console.Command  as CP
import qualified System.Console.Program  as CP
import           System.Random.Shuffle (shuffleM)

import           Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Text.Read           (readMaybe)

main :: IO ()
main = do
  globalStore <- DB.initialiseGlobal
  accountID <- input
  store <- snd <$> DB.accountStore accountID Map.empty
  CP.interactive $ commands globalStore store

-- The tree of possible commands of the program.
commands :: DB.Store GlobalS -> DB.Store AccountS -> CP.Commands IO
commands globalStore store = Node
  (CP.command "DB" "Manage the database." . CP.io . CP.showUsage $ commands globalStore store)
  [
    Node (CP.command "list" "" . CP.io . CP.showUsage $ commands globalStore store)
      [ Node (CP.command "questions" "List questions." $ CP.io . (>>= mapM_ print) . DB.runIn store $
          (ID.listWithID :: DB.DB [ID.WithID (Decorated Question)])
        ) []
      , Node (CP.command "tests" "List tests." $ CP.io . (>>= mapM_ print) . DB.runIn store $
          (ID.listWithID :: DB.DB [ID.WithID (Decorated Test)])
        ) []
      ]
  , Node (CP.command "export" "" . CP.io . CP.showUsage $ commands globalStore store)
      [ flip Node [] $ CP.command "questions" "Export questions." $ CP.io $
        (>>= writeFile "questions.hs" . show) . DB.runIn store $
        (ID.listWithID :: DB.DB [ID.WithID (Decorated Question)])
      ]
  , flip Node [] $ CP.command "import" "" . CP.io $ do
      importOld store
  , Node (CP.command "show" "" . CP.io . CP.showUsage $ commands globalStore store)
      [ Node (CP.command "question" "Show a specific question." $
          CP.withNonOption idArg $ \ (i :: ID.ID (Decorated Question)) -> CP.io
            . (>>= maybe (putStrLn "Question not found.") print)
            . DB.runIn store $ ID.lookupMaybe i
        ) []
      , Node (CP.command "test" "Show a specific test." $
          CP.withNonOption idArg $ \ (i :: ID.ID (Decorated Test)) -> CP.io
            . (>>= maybe (putStrLn "Test not found.") print)
            . DB.runIn store $ ID.lookupMaybe i
        ) []
      ]
  , Node (CP.command "undelete" "" . CP.io . CP.showUsage $ commands globalStore store)
      [ Node (CP.command "question" "Undelete a question." $
          CP.withNonOption idArg $ \ (i :: ID.ID (Decorated Question)) -> CP.io $ do
            m <- DB.runIn store $
              overM (ID.refLens . authored . labelled) undeleteDated
                =<< ID.refM i
            putStrLn . maybe "Failed" (const "Succeeded") $ m
        ) []
      , Node (CP.command "test" "Undelete a test." $
          CP.withNonOption idArg $ \ (i :: ID.ID (Decorated Test)) -> CP.io $ do
            m <- DB.runIn store $
              overM (ID.refLens . authored . labelled) undeleteDated
                =<< ID.refM i
            putStrLn . maybe "Failed" (const "Succeeded") $ m
        ) []
      ]
  , Node (CP.command "new" "" . CP.io . CP.showUsage $ commands globalStore store)
      [ Node (CP.command "question" "Add a new question." . CP.io
          $ (DB.runIn store . ID.addNew =<< (input :: IO (Decorated Question)))
            >> putStrLn "Question added."
          ) []
      , Node (CP.command "test" "Add a new test." . CP.io
          $ (DB.runIn store . ID.addNew =<< (input :: IO (Decorated Test)))
            >> putStrLn "Test added."
          ) []
      , Node (CP.command "account" "Add a new account." . CP.io
          $ (DB.runIn globalStore . ID.addNew =<< (input :: IO Account))
            >> putStrLn "Account added."
          ) []
      , Node (CP.command "role" "Add a new role." . CP.io
          $ (DB.runIn globalStore . T.newDBRef =<< (input :: IO Role))
            >> putStrLn "Role added."
          ) []
      ]
  , Node (CP.command "add" "" . CP.io . CP.showUsage $ commands globalStore store)
    [ Node (CP.command "autoaccount" "Mark an account as to be added to new users automatically." . CP.io
       $ (DB.runIn globalStore . addAuto =<< (input :: IO (ID.ID Account)))
         >> putStrLn "Account marked."
       ) []
    ]
  ]

importOld :: Store AccountS -> IO ()
importOld store = do
  user <- input
  DB.runIn store $ mapM_ T.newDBRef $ map (addAuthor user) OldData.questions
  DB.runIn store $ mapM_ T.newDBRef $ map (addAuthor user) OldData.tests
 where
  addAuthor :: Author -> ID.WithID x -> ID.WithID (Authored x)
  addAuthor user = over ID.object $ Authored user

addAuto :: ID.ID Account -> DB.DB ()
addAuto i = do
  d <- T.getDBRefM ""
  s <- maybe Set.empty getAutoAccounts <$> T.readDBRef d
  T.writeDBRef d . AutoAccounts $ Set.insert i s

idArg :: CP.Type (ID.ID a)
idArg = ID.ID . pack <$> CP.string

class Input x where
  input :: IO x

instance Input Question where
  input = do
    q <- putStrLn "Question:" >> input
    a <- putStrLn "Answer:" >> input
    let title = generateTitle q
    putStrLn "Enter more answers for multiple choice, empty line when done:"
    others <- map plainRich <$> askMany
    if null others
      then return $ Question q (Open a) title
      else do
        let answers = (True, a) : map ((,) False) others
        randomOrder <- shuffleM [0 .. length others]
        return $ Question q (MultipleChoice answers randomOrder) title

instance Input Test where
  input = do
    name <- prompt "Name:"
    qs <- map (TestQuestion . ID.ID . pack) <$> promptF read "Questions:"
    return $ Test name qs

instance (Input x) => Input (Dated x) where
  input = date =<< input

instance (Input x) => Input (Labelled x) where
  input = do
    x <- input
    putStrLn "Labels:"
    ls <- Set.fromList <$> askMany
    return $ Labelled ls x

instance (Input x) => Input (Authored x) where
  input = do
    x <- input
    putStrLn "Author:"
    u <- input
    return $ Authored u x

instance Input Account where
  input = promptF (Account . pack) "Enter account name:"

instance Input Role where
  input = do
    u <- input
    p <- input
    return $ Role u p

instance Input Privilege where
  input = do
    mf <- pickFrom
      [ (const GlobalAdministrator, "Global administrator"        )
      , (      Administrator      , "Administrator for an account")
      , (      Editor             , "Editor for an account")
      ]
    case mf of
      Nothing -> input
      Just f  -> do
        a <- input
        return $ f a

instance Input (ID.ID Account) where
  input = promptF (ID.ID . pack) "Enter account ID:"

instance Input (ID.ID User) where
  input = promptF (ID.ID . pack) "Enter internal user ID:"

instance Input RichText where
  input = plainRich <$> input

instance Input Text where
  input = pack <$> getLine

prompt :: String -> IO Text
prompt = promptF pack

promptF :: (String -> a) -> String -> IO a
promptF f s = do
  putStrLn s
  f <$> getLine

askMany :: IO [Text]
askMany = do
  x <- getLine
  if null x
    then return []
    else (pack x :) <$> askMany


-- TODO: also in PackingAid; factor this out into a new package console-input
pickFrom :: [(a, Text)] -> IO (Maybe a)
pickFrom []      = outputLine "No choices." >> return Nothing
pickFrom [(x, t)] = return (Just x)
pickFrom cs      = do
  outputLine "Choose from:"
  let withNumber = zip [1 ..] cs
  flip mapM_ withNumber $ \ (i, (_a, text)) -> outputOption i text
  askNumber withNumber
 where
  askNumber :: [(Int, (a, Text))] -> IO (Maybe a)
  askNumber xs = inputLine "" >>= \case
    Nothing   -> return Nothing
    Just line -> case readMaybe line of
      Nothing  -> outputLine "parse error of number" >> askNumber xs
      Just j   -> case lookup j xs of
        Nothing        -> outputLine "option out of range" >> askNumber xs
        Just (a, text) -> do
          -- liftIO ANSI.clearLine
          -- let offset = length xs - j + 1
          -- liftIO $ ANSI.cursorUp $ offset + 1
          -- liftIO ANSI.clearLine
          -- bold $ outputOption j text
          -- liftIO $ ANSI.cursorDown $ offset - 1
          return $ Just a
  outputOption i t = outputLine $ Text.pack (show i) <> ") " <> t

outputLine :: Text -> IO ()
outputLine = Text.putStrLn

inputLine :: String -> IO (Maybe String)
inputLine _ = Just <$> getLine
