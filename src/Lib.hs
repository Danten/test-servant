{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
module Lib
    ( runApp
    ) where

import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Lazy     as BS
import qualified Data.List.NonEmpty       as NE
import           Data.Monoid              ((<>))
import           Data.Proxy
import           Data.Text
import qualified Data.Text.Encoding       as Text
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Servant
import           Servant.API


data HTML

instance Accept HTML where
  contentTypes _ = "text/html" NE.:| ["application/xhtml+xml"]

instance MimeRender HTML Text where
  mimeRender _ = BS.fromStrict . Text.encodeUtf8

type Api =
  "api" :>
   (Get '[JSON] Text :<|>
    Post '[JSON] Text) :<|>
  "index" :> Get '[HTML] Text :<|>
  "static" :> Raw

api :: Proxy Api
api = Proxy

tag :: Text -> [Text] -> Text
tag name = tagA name []

tagS :: Text -> Text -> Text
tagS name text = tag name [text]

tagA :: Text -> [(Text, Text)] -> [Text] -> Text
tagA name attributes children = "<" <> name <>
  mconcat [ " " <> att <> "=\"" <> val <> "\"" | (att, val) <- attributes] <> ">"
  <> mconcat children <> "</" <> name <> ">"

tagAS :: Text -> [(Text, Text)] -> Text
tagAS name attributes = tagA name attributes []

(-->) :: a -> b -> (a, b)
a --> b = (a,b)

lorem0 :: Text
lorem0 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris a egestas lacus. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Pellentesque mattis est rhoncus mi tincidunt, vel ultricies nisi luctus. Fusce sollicitudin lectus sit amet turpis lobortis, nec mattis sapien tempor. Donec tempus urna sit amet mauris efficitur, at gravida orci ullamcorper. Aliquam commodo, magna sit amet condimentum convallis, lectus magna dignissim ex, in vehicula augue justo at lectus. Ut augue est, tincidunt sit amet molestie ac, rutrum ac est."

lorem1 :: Text
lorem1 = "Nulla a sem a metus lacinia bibendum sit amet et purus. Aliquam sollicitudin neque in sodales congue. Nam vitae efficitur purus. Vestibulum et finibus arcu, vitae ultrices nunc. Vivamus ullamcorper vel purus pretium cursus. In vitae ante nec nulla tristique pretium. Pellentesque molestie libero at tortor mattis, vel aliquam mi imperdiet."

lorem2 :: Text
lorem2 = "Etiam elementum sit amet sapien eu rutrum. Etiam justo lacus, cursus sed malesuada et, vehicula et mauris. Phasellus laoreet quis enim eu pulvinar. Praesent nec odio vel lectus gravida efficitur. Aenean vulputate mi at magna aliquam molestie. Donec elementum aliquam maximus. Quisque lectus metus, viverra a urna id, varius scelerisque augue. In hac habitasse platea dictumst. Cras ac nunc ut arcu scelerisque interdum in in arcu. Sed tellus tellus, lobortis ac metus eu, dapibus consequat nisi. Suspendisse orci augue, commodo at fringilla in, blandit tempor dolor. Suspendisse fringilla ornare turpis, vel laoreet neque sagittis ut. Integer facilisis libero a diam commodo, in volutpat dolor porttitor."

html :: Text
html = tag "html"
  [ tag "head"
    [ tagAS "meta" ["charset" --> "UTF-8"]
    , tagS "title" "My cool website"
    , tagAS "meta" [ "name" --> "description"
                   , "content" --> "Supposed to be some sort of portal?"]
    , tagAS "link" [ "rel" --> "stylesheet"
                   , "href" --> "static/style.css"
                   ]
    ]
  , tag "body"
    [ tag "header"
      [ tagS "h1" "Oh yeah this is awesome!" ]
    , tagA "div" ["class" --> "sidebar"]
      [ tagS "h2" "This should be some list"
      , tag "ul"
        [ tagS "li" "First entry"
        , tagS "li" "Second entry"
        ]
      ]
    , tagA "div" ["class" --> "main"]
      [ tagS "h2" "We can totally make some interface out of this!"
      , tagS "p" "So we could probably do something like this"
      , tagA "form" ["action" --> "/api", "method" --> "post"]
        [ tagAS "input" [ "type" --> "text"
                        , "name" --> "state"]
        , tagAS "input" [ "type" --> "button"
                        , "value" --> "Submit (probably)"
                        , "onclick" --> "myFunction()"]
        ]
      , tagS "p" lorem0
      , tagS "p" lorem1
      , tagS "p" lorem2
      ]
    , tag "footer"
      [ tagS "p" "Copyright ⓒ Daniel Gustafsson 2017... or something.."
      , tagS "p" "I mean why would anyone want to copy this???"]
    , tagAS "script" [ "type" --> "text/javascript"
                     , "src" --> "static/test.js"
                     ]
    ]
  ]

postIt :: Handler Text
postIt = do
  liftIO $ putStrLn "we did a post!!"
  return "post"

server :: Server Api
server = (return "Hello, I'm a server"
  :<|> postIt)
  :<|> return html
  :<|> serveDirectoryWebApp "/home/danten/haskell/test-servant/static"

app :: Application
app = serve api server

runApp :: IO ()
runApp = do
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8080 $ setLogger aplogger defaultSettings
    runSettings settings app
