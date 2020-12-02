module Config where

import Multilang
import Hakyll (FeedConfiguration(..), constField)

rootHost = "http://vimly.info"

data CourseMetasConfiguration = CourseMetasConfiguration
  { -- | Title
    cmcTitle       :: String
  , -- | Image Cover
    cmcImgCover    :: String
  , -- | Description
    cmcDescription :: String
  , -- | Twitter Creator
    cmcTwitter     :: String
  } deriving (Show, Eq)

proVersion  = [
                  constField "versionW" "426c7fc212"
                , constField "versionD" "865813558f"
              ]
freeVersion = [
                  constField "versionW" "23af82aff2"
                , constField "versionD" "d8c7673dbb"
              ]

feedConfig :: Language -> FeedConfiguration
feedConfig _ = FeedConfiguration
  { feedTitle       = "Vimly"
  , feedDescription = "Learning Sessions Available"
  , feedAuthorName  = "Giacomo Mantani"
  , feedAuthorEmail = "starly.info@gmail.com"
  , feedRoot        = "http://vimly.info"
  }

vimlyConfig :: CourseMetasConfiguration
vimlyConfig = CourseMetasConfiguration
  { cmcTitle        = "VimLy"
  , cmcImgCover     = "https://vimly.github.io/static/img/fb-cover.png"
  , cmcDescription  = "Vimly: A New Way of Learning The Vim Manual Directly in Your Inbox"
  , cmcTwitter      = "@Vim_StarLy"
  }
