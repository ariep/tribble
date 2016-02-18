module Export.Pandoc
  (
    build
  , export
  ) where

import qualified DB
import           Common
import           Config
import           Imports
import           Types

import           Data.ByteString.Lazy (ByteString)
import           Data.List            (sortOn)
import qualified Data.Map                as Map
import qualified Data.TCache.ID          as ID
import qualified Data.Text               as Text
import qualified Text.Pandoc             as P
import qualified Text.Pandoc.Builder     as P
import qualified Text.Pandoc.Walk        as P
import qualified Web.Channel.Server      as S


export :: (Monad m) => P.Pandoc -> m (Either ByteString ByteString)
export = return . Right . utf8ByteString . P.writeNative P.def

build :: ExportMode -> Decorated Test -> DB.DB P.Pandoc
build mode test = do
  qts <- for (view (undecorated . elements) test) $ \case
    TestQuestion i -> return . Right . view (ID.object . undecorated)
      =<< ID.lookup i
    TestText t     -> return $ Left t
  return $ renderTest mode (view (undecorated . name) test) qts

renderTest :: ExportMode -> Text -> [Either RichText Question] -> P.Pandoc
renderTest mode name qs = P.setTitle (textP name) . P.doc $
  mconcat . map (either renderText renderQuestion) . numberRight $ qs
 where
  numberRight :: [Either a b] -> [Either a (Int, b)]
  numberRight xs = go 1 xs where
    go _ []             = []
    go i (Left x  : xs) = Left x       : go i        xs
    go i (Right x : xs) = Right (i, x) : go (succ i) xs
  renderText :: RichText -> P.Blocks
  renderText (Pandoc b) = P.fromList b
  renderQuestion :: (Int,Question) -> P.Blocks
  renderQuestion (i, q) = let Pandoc b = view question q in
    P.orderedListWith (i, P.Decimal, P.Period) . (: []) $
      P.fromList b <> case view answer q of
        Open (Pandoc a)       -> case mode of
          OnlyQuestions -> mempty
          WithAnswers   -> P.fromList $ strong a
        m@(MultipleChoice {}) -> P.orderedListWith
          (1, P.UpperAlpha, P.OneParen) $ map renderChoice answers
         where
          answers :: [(Bool, RichText)]
          -- answers = view choices m `orderBy` view order m
          answers = sortOn (textLength . snd) $ view choices m
          renderChoice :: (Bool,RichText) -> P.Blocks
          renderChoice (corr,Pandoc t) = P.fromList $ case mode of
            OnlyQuestions -> t
            WithAnswers   -> (if corr then strong else strikeout) t

strong :: [P.Block] -> [P.Block]
strong = map . P.walk $ P.Strong . (: [])

strikeout :: [P.Block] -> [P.Block]
strikeout = map . P.walk $ P.Strikeout . (: [])

orderBy :: [a] -> AnswerOrder -> [a]
orderBy = map . (!!)

textLength :: RichText -> Int
textLength = Text.length . renderPlain

textP :: Text -> P.Inlines
textP = P.str . Text.unpack
