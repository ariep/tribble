module Services where

import           Common
import           Components
import qualified DB
import qualified Export
import           Imports
import           Types

import qualified Web.Channel                    as Ch
import qualified Web.Channel.Server             as S
import qualified Web.Channel.Server.Session     as S

import qualified Control.Concurrent.Notify      as Notify
import qualified Control.Coroutine.Monadic      as Co
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.TCache.ID                 as ID
import qualified Data.TCache.Index.Text         as IndexText
import qualified System.Directory               as Dir
import           System.Random           (newStdGen, randomRs)
import qualified Web.OAuth2.Google              as OAuth

channels :: State -> IO [S.ServedChannel]
channels state = do
  n₁ <- Notify.subscribe $ changed state
  n₂ <- Notify.subscribe $ changed state
  return $ 
    crud state n₁ crudTests (Proxy :: Proxy (Decorated Test)) ++
    crud state n₂ crudQuestions (Proxy :: Proxy (Decorated Question)) ++
    [ S.ServeChannel showUser $
        Co.pre S.getUser $ \ user ->
        Co.put (OAuth.name user) Co.>>
        Co.close
    , S.ServeChannel uploadImage $ Co.repeat $
        Co.get Co.>>= \ (Ch.File b) ->
        Co.pre (liftIO $ saveFileRandomName "/images/" b) $ \ fn ->
        Co.put fn Co.>>
        Co.close
    , S.ServeChannel exportTest $ Co.repeat $
        Co.get Co.>>= \ (testID, mode, format) ->
        Co.pre (dbRun state False $ Export.export mode format testID) $ \ r ->
        Co.put (remoteUrl <$> r) Co.>> Co.close
    , S.ServeChannel filterQuestions $ Co.repeat $
        Co.get Co.>>= \ qFilter ->
        Co.pre (dbRun state False $
          map (\ (k, _, _) -> ID.fromKey k) <$>
            IndexText.lookup DB.questionIndex 10 qFilter
        ) $ \ qs ->
        Co.put qs Co.>> Co.close
    ]

crud :: forall a. (SafeCopy a, Typeable a, Show a)
  => State -> Notify.Listen -> Ch.Token (Ch.CRUD a) -> Proxy a ->
  [S.ServedChannel]
crud state notify t _ =
  [ S.ServeChannel (Ch.list t :: Ch.Channel (Ch.List a)) $ Co.repeat $
      Co.pre  (dbRun state False DB.getAll :: S.M [ID.WithID a]) $ \ ls ->
      Co.put ls Co.>>
      Co.pre_ (liftIO $ Notify.wait notify)
      Co.close
  , S.ServeChannel (Ch.delete t :: Ch.Channel (Ch.Delete a)) $ Co.repeat $
      Co.get Co.>>= \ i ->
      Co.pre_ (dbRun state True $ ID.delete =<< ID.refM i)
      Co.close
  , S.ServeChannel (Ch.create t :: Ch.Channel (Ch.Create a)) $ Co.repeat $
      Co.get Co.>>= \ (x :: a) ->
      Co.pre (dbRun state True $ ID.addNew x) $ \ i ->
      Co.put i Co.>>
      Co.close
  , S.ServeChannel (Ch.update t) $ Co.repeat $
      Co.get Co.>>= \ (x :: ID.WithID a) ->
      Co.pre_ (dbRun state True $ ID.write x >> return ())
      Co.close
  ]

saveFileRandomName :: FilePath -> BSL.ByteString -> IO FilePath
saveFileRandomName dir b = do
  (f, localFile) <- newName
  BSL.writeFile localFile b
  return f
 where
  randomName = take 16 . randomRs ('a', 'z') <$> newStdGen
  newName = do
    f <- (dir ++) <$> randomName
    let localFile = "./upload" ++ f
    Dir.doesFileExist localFile >>= \case
      True  -> newName
      False -> return (f, localFile)

