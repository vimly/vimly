module Config where

import Multilang
import Hakyll (FeedConfiguration(..), constField)

rootHost = "https://starly-info.github.io"

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
  { feedTitle       = "starly"
  , feedDescription = "Learning Sessions Available"
  , feedAuthorName  = "Giacomo Mantani"
  , feedAuthorEmail = "name.surname at gmail"
  , feedRoot        = "https://starly-info.github.io"
  }

vimlyConfig :: CourseMetasConfiguration
vimlyConfig = CourseMetasConfiguration
  { cmcTitle        = "VimLy"
  , cmcImgCover     = "https://starly-info.github.io/static/img/fb-cover.png"
  , cmcDescription  = "Vim Knowledge and Practice in Your Inbox.. Daily or Weekly!"
  , cmcTwitter      = "@Vim_StarLy"
  }
