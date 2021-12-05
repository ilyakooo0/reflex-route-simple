module Main (main) where

import Control.Monad
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Reflex.Dom
import Reflex.Dom.Route.Simple
import Text.Lorem

data BlogRoutes domain
  = IndexR
  | ArticleR (domain :# "article" :/ ArticleId)
  | CommentR (domain :# "article" :/ ArticleId :/ "comment" :/ CommentId)
  deriving stock (Generic)

newtype ArticleId = ArticleId Int
  deriving newtype (Eq, Ord, ToHttpApiData, FromHttpApiData)

newtype CommentId = CommentId Int
  deriving newtype (Eq, Ord, ToHttpApiData, FromHttpApiData)

data Comment = Comment
  { commentAuthorName :: Text,
    commentBody :: Text
  }

data Article = Article
  { articleTitle :: Text,
    articleBody :: Text,
    articleComments :: Map CommentId Comment
  }

newtype Model = Model
  {articles :: Map ArticleId Article}

generateModel :: IO Model
generateModel = fmap (Model . mapify ArticleId) $
  replicateM 7 $ do
    Article <$> natural_sentence <*> paragraph 5 10
      <*> fmap
        (mapify CommentId)
        (replicateM 10 $ Comment <$> natural_word <*> paragraph 1 3)
  where
    mapify :: Ord k => (Int -> k) -> [v] -> Map k v
    mapify f = M.fromList . zip (f <$> [1 ..])

articlesW :: (MonadWidget t m, MonadRoute BlogRoutes t m) => Model -> m ()
articlesW (Model as) =
  el "ul" $
    forM_ (M.toList as) $ \(aId, a) ->
      el "li" $ do
        Link clicked <- link (articleTitle a)
        rerouteEvent $ clicked $> ArticleR (Proxy :/ aId)

articleW :: (MonadWidget t m, MonadRoute BlogRoutes t m) => ArticleId -> Article -> m ()
articleW aId a = do
  el "h2" $ text $ articleTitle a
  text $ articleBody a
  el "h3" $ text "Comments"
  el "ul" $
    forM_ (M.toList $ articleComments a) $ \(cId, c) ->
      el "li" $ do
        (Link clicked) <- link (commentBody c)
        rerouteEvent $ clicked $> CommentR (Proxy :/ aId :/ Proxy :/ cId)

commentW :: MonadWidget t m => Article -> Comment -> m ()
commentW a c = do
  el "h4" $ text $ "Comment on article \"" <> articleTitle a <> "\""
  el "h3" $ text $ "Comment author: " <> commentAuthorName c
  text $ commentBody c

headW :: MonadWidget t m => m ()
headW = do
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") $ pure ()
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://unpkg.com/awsm.css/dist/awsm.min.css") $ pure ()

bodyW :: MonadWidget t m => Model -> m ()
bodyW model = do
  el "header" $ el "h1" $ text "My awesome blog"
  el "main" $ runRouteT' \case
    Nothing -> articlesW model
    Just IndexR -> articlesW model
    Just (ArticleR (Proxy :/ aId)) -> articleW aId $ articles model M.! aId
    Just (CommentR (Proxy :/ aId :/ Proxy :/ cId)) ->
      let a = articles model M.! aId
          c = articleComments a M.! cId
       in commentW a c
  pure ()

main :: IO ()
main = do
  model <- generateModel
  mainWidgetWithHead headW (bodyW model)
