{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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
import           Control.Monad           (filterM)
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.Search.Results            as Results
import qualified Data.Set                       as Set
import qualified Data.TCache.ID                 as ID
import qualified Data.TCache.Index.Map          as IndexMap
import qualified Data.TCache.Index.Text         as IndexText
import qualified Data.Text.Index                as Index
import qualified System.Directory               as Dir
import           System.Random           (newStdGen, randomRs)
import qualified Web.OAuth2.Google              as OAuth

import qualified Data.Text as Text

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
    , queryService state filterQuestions searchQuestions
    , queryService state questionLabels searchQuestionLabel
    ]

queryService :: (SafeCopy a, SafeCopy b) =>
  State ->
  Ch.Channel (Co.Repeat (a Co.:?: b Co.:!: Co.Eps)) ->
  (a -> DB.DB b) ->
  S.ServedChannel
queryService state c q = S.ServeChannel c $ Co.repeat $
  Co.get Co.>>= \ a ->
  Co.pre (dbRun state False $ q a) $ \ b ->
  Co.put b Co.>> Co.close

-- pureService :: (SafeCopy a, SafeCopy b) =>
--   Ch.Channel (Co.Repeat (a Co.:?: b Co.:!: Co.Eps)) ->
--   (a -> b) ->
--   S.ServedChannel
-- pureService c f = S.ServeChannel c $ Co.repeat $
--   Co.get Co.>>= \ a ->
--   Co.put (f a) Co.>> Co.close

crud :: forall a b. (SafeCopy b, Typeable b, a ~ Decorated b)
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
  , S.ServeChannel (Ch.markDeleted t) $ Co.repeat $
      Co.get Co.>>= \ i ->
      Co.pre_ (dbRun state True $ do
        d <- liftIO getCurrentTime
        overM (ID._IDLens . labelled) (deleteDated d) i
        return ())
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

mapWeight :: Index.Weight -> Rational
mapWeight = max 0 . (1 -) . (/ 10) . toRational . Index.toDouble

searchQuestions :: QuestionFilter ->
  DB.DB [(ID.ID (Decorated Question), Rational)]
searchQuestions qFilter = case view searchText qFilter of
  Nothing -> fmap (map $ flip (,) 1 . view ID._ID) $
    fmap (filter relevant) . (mapM ID.lookup =<<) $ DB.getAllIDs
  Just t  -> fmap (map $ \ (k, w, _) -> (ID.fromKey k, mapWeight w)) .
    fmap (Results.toList) .
    (Results.filterM (fmap relevant . ID.lookup . ID.fromKey . fst) =<<) $
    IndexText.lookup DB.questionIndex t
 where
  undeleted, rightLabels, relevant :: ID.WithID (Decorated Question) -> Bool
  undeleted = not . isDeleted . view (ID.object . labelled)
  rightLabels = (view labelFilter qFilter `Set.isSubsetOf`) .
    view (ID.object . labels)
  relevant w = undeleted w && rightLabels w

searchQuestionLabel :: Maybe Text -> DB.DB [Label]
searchQuestionLabel Nothing  = map fst <$> IndexMap.listAll DB.questionLabelIndex
searchQuestionLabel (Just t) = map (\ (_, _, l) -> l) . Results.take 10 <$>
  IndexText.lookup DB.questionLabelTextIndex t
