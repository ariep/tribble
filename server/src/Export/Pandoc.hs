module Export.Pandoc
  ( P.Pandoc
  , build
  , export
  ) where

import qualified DB
import           Common
import           Config
import           Export.Common (Container, Result)
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


export :: (Monad m) =>
  Container -> P.Pandoc -> m Result
export _ = return . Right . utf8ByteString . P.writeNative P.def

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
  renderText = P.fromList . createPandoc
  renderQuestion :: (Int, Question) -> P.Blocks
  renderQuestion (i, q) = P.orderedListWith (i, P.Decimal, P.Period) . (: []) $
    P.fromList (createPandoc $ view question q) <> case view answer q of
      Open a                -> case mode of
        OnlyQuestions -> mempty
        WithAnswers   -> P.fromList $ strong $ createPandoc a
      m@(MultipleChoice {}) -> P.orderedListWith
        (1, P.UpperAlpha, P.OneParen) $ map renderChoice answers
       where
        answers :: [(Bool, RichText)]
        -- answers = view choices m `orderBy` view order m
        answers = sortOn (textLength . snd) $ view choices m
        renderChoice :: (Bool, RichText) -> P.Blocks
        renderChoice (corr, t) = P.fromList $ case mode of
          OnlyQuestions -> createPandoc t
          WithAnswers   -> (if corr then strong else strikeout) $ createPandoc t

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

createPandoc :: RichText -> [P.Block]
createPandoc = either (return . P.Plain . return . P.Str . show) r .
  P.readHtml P.def . Text.unpack . rtToHtml
    -- The below alternative gives a runtime error in the browser!
    -- (Pandoc.def
    --   { Pandoc.readerSmart = True
    --   }
    -- :: Pandoc.ReaderOptions)
 where
  r (P.Pandoc _ p) = p
