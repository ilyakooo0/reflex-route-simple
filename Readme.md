# 🚦 reflex-route-simple

A simple type-safe routing framework for reflex-dom applications without any `TemplateHaskell`.

## How it works

### Defining routes

> 👀 You can see a full example in [`app/Main.hs`](./app/Main.hs).
>
> 🌐 You can see the example in action at [iko.soy/reflex-route-simple](https://iko.soy/reflex-route-simple/).

You only need to import a single module:

```haskell
import Reflex.Dom.Route.Simple
```

Your routes are a single sum-type with your domain as a polymorphic parameter:

```haskell
data BlogRoutes domain
  = IndexR
  | ArticleR (domain :# "article" :/ ArticleId)
  | CommentR (domain :# "article" :/ ArticleId :/ "comment" :/ CommentId)
  deriving stock (Generic)
```

Every constructor should have either:
1. Zero arguments. This route is interpreted as the root of your app – `/`.
2. A single argument. The argument should be a path.

A path always starts with `domain :#`. You then need to have at least one path piece.

A path piece can be either:
1. A `Symbol` – a type-level string. This will be interpreted as a static path piece.
2. A type. This type will be interpreted as a dynamic path piece. The type needs to be an instance of [`FromHttpApiData`](https://hackage.haskell.org/package/http-api-data-0.4.3/docs/Web-HttpApiData.html#t:FromHttpApiData) and [`ToHttpApiData`](https://hackage.haskell.org/package/http-api-data-0.4.3/docs/Web-HttpApiData.html#t:ToHttpApiData) from the [`http-api-data`](https://hackage.haskell.org/package/http-api-data) library (the same one [`servant`](https://haskell-servant.github.io) uses).

### Running routes

All routes are matched agains in the order that the constructors appear in.

To actually "run" the routes you need to call `runRouteT` (or one of his friends) where `r` is the routes sum-type we created earlier – `BlogRoutes`:

```haskell
runRouteT    ::                           (r RouteValue -> m a) -> m (Event t a)
runRouteT'   ::                           (r RouteValue -> m a) -> m ()
runRouteTEv  :: Event t (r RouteValue) -> (r RouteValue -> m a) -> m (Event t a)
runRouteTEv' :: Event t (r RouteValue) -> (r RouteValue -> m a) -> m ()
```


As you can see, to run your routes you need to create a function with the type `r RouteValue -> m a`. In our case it would take a `BlogRoutes RouteValue` and return the page to display.

### What is `RouteValue`?

When we apply `RouteValue` to your routes type we get the same type, but the types are turned into values:
- `Symbol`s (type-level strings) are transformed into `Proxy`s. (There is not way to create a value of type `Symbol`.)
- Any other type is turned into a value of that type.

So, we can now create a route as a value!

```haskell
favoriteArticleRoute :: BlogRoutes RouteValue
favoriteArticleRoute = ArticleR (Proxy :/ 7)

criticalCommentRoute :: BlogRoutes RouteValue
criticalCommentRoute = CommentR (Proxy :/ 6 :/ Proxy :/ 2)
```

The `Proxy`s are useful because you can over over them in a sufficiently smart editor and see what static parts they are so that you don't get lots (: .

### Let's run the routes!

We can now run our routes, supplying a lambda that looks through all possible routes and decides what page should be displayed. You can pattern-match on the exact same structures we used earlier to create the routes:

```haskell
bodyW :: MonadWidget t m => Model -> m ()
bodyW = runRouteT' \case
  Nothing -> articlesW
  Just IndexR -> articlesW
  Just (ArticleR (Proxy :/ articleId)) -> articleW articleId
  Just (CommentR (Proxy :/ articleId :/ Proxy :/ commentId)) ->
    commentW articleId commentId
```

The `Nothing` represents an "incorrect" path (a path that couldn't be parsed).


### How to switch routes?

All of the pages returned after selecting routes have access to the `MonadRoute` typeclass:

```haskell
class MonadRoute r t m where
  reroute :: r RouteValue -> m ()
  rerouteEvent :: Event t (r RouteValue) -> m ()
```

As you probably guessed, the `r` is the Routes structure you defined earlier! This means that inside the pages have access to this function:

```haskell
rerouteEvent :: Event t (BlogRoutes RouteValue) -> m ()
```

You can just create a route value and attach to an event. For example, when a user clicks a button or a link.

Let's try it out! We will look at `articleW` defined earlier:

```haskell
articlesW :: (MonadWidget t m, MonadRoute BlogRoutes t m) => m ()
articlesW = do
  articles <- getAllArticles
  forM_ articles $ \(articleId, articleTitile) ->
    Link clicked <- link articleTitile
    rerouteEvent $ clicked $> ArticleR (Proxy :/ articleId)
```

We define a link for every article and when the user clicks the link, the selected article is opened!
