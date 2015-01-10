{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Web.Haskbot.Types where

import Data.Aeson (ToJSON, Value (..), (.=), object, toJSON)
import Data.Monoid ((<>))
import Network.URI (URI)
import Web.Scotty.Trans (Parsable)

import qualified Data.Text as ST

class Formatted a where
  formatted :: a -> ST.Text

-- outgoing integrations

data SlashCommand = SlashCommand
  { sc_token    :: !Token
  , sc_teamID   :: !TeamID
  , sc_chanID   :: !ChannelID
  , sc_chanName :: !ChannelName
  , sc_userID   :: !UserID
  , sc_userName :: !UserName
  , sc_command  :: !Command
  , sc_optText  ::  Maybe ST.Text
  } deriving (Eq, Show)

data Outgoing = Outgoing
  { out_token       :: !Token
  , out_teamID      :: !TeamID
  , out_chanID      :: !ChannelID
  , out_chanName    :: !ChannelName
  , out_timestamp   :: !Timestamp
  , out_userID      :: !UserID
  , out_userName    :: !UserName
  , out_text        :: !ST.Text
  , out_triggerWord :: !TriggerWord
  } deriving (Eq, Show)

-- incoming integration

data Incoming = Incoming
  { inc_channel  :: !ResponseAddr
  , inc_text     :: !SlackText
  } deriving (Eq, Show)

instance ToJSON Incoming where
  toJSON inc = object [ "channel" .= inc_channel inc
                      , "text"    .= inc_text inc
                      ]

data ResponseAddr = DirectMsg !UserName
                  | Channel   !ChannelName
                  deriving (Eq, Show)

instance ToJSON ResponseAddr where
  toJSON (DirectMsg d) = toJSON d
  toJSON (Channel c)   = toJSON c

data SlackText =
    Plain ST.Text SlackText
  | URL URI SlackText
  | Link URI ST.Text SlackText
  | End
  deriving (Eq, Show)

instance Formatted SlackText where
  formatted slackText = case slackText of
    (Plain text next) ->
      text <> formatted next
    (URL uri next) ->
      "<" <> ST.pack (show uri) <> ">" <> formatted next
    (Link uri text next) ->
      "<" <> ST.pack (show uri) <> "|" <> text <> ">" <> formatted next
    End -> ""

instance ToJSON SlackText where
  toJSON slackText = String $ formatted slackText

--data Incoming = Incoming
--  { inc_channel  :: !ResponseAddr
--  , inc_text     :: !ST.Text
--  , inc_fallback :: !ST.Text
--  , inc_pretext  :: !ST.Text
--  , inc_color    ::  Maybe Color
--  , inc_fields   ::  [Attachment]
--  , inc_userName ::  UserName
--  , inc_icon     ::  Icon
--  } deriving (Eq, Show)

--data Color = Good
--           | Warning
--           | Danger
--           | Hex HexCode
--           deriving (Eq, Show)
--
--instance ToJSON Color where
--  toJSON Good    = String "good"
--  toJSON Warning = String "warning"
--  toJSON Danger  = String "danger"
--  toJSON (Hex h) = toJSON h
--
--
--data Attachment = Attachment
--  { at_title :: !ST.Text
--  , at_value :: !ST.Text
--  , at_short :: !Bool
--  } deriving (Eq, Show)
--
--instance ToJSON Attachment where
--  toJSON att = object [ "title" .= at_title att
--                      , "value" .= at_value att
--                      , "short" .= at_short att
--                      ]
--
--
--data Icon = IconURL URI
--          | IconEmoji Emoji
--          deriving (Eq, Show)
--
--instance ToJSON URI where
--  toJSON = String . ST.pack . show
--
---- private functions
--
--iconPair :: Icon -> (ST.Text, Value)
--iconPair (IconURL url)   = "icon_url"   .= url
--iconPair (IconEmoji emo) = "icon_emoji" .= emo

-- newtypes

newtype Token       = Token ST.Text       deriving (Eq, Show, ToJSON, Parsable)
newtype TeamID      = TeamID ST.Text      deriving (Eq, Show, ToJSON, Parsable)
newtype ChannelID   = ChannelID ST.Text   deriving (Eq, Show, ToJSON, Parsable)
newtype UserID      = UserID ST.Text      deriving (Eq, Show, ToJSON, Parsable)
newtype Timestamp   = Timestamp ST.Text   deriving (Eq, Show, ToJSON, Parsable)
newtype TriggerWord = TriggerWord ST.Text deriving (Eq, Show, ToJSON, Parsable)
newtype ChannelName = ChannelName ST.Text deriving (Eq, Show, ToJSON, Parsable)
newtype UserName    = UserName ST.Text    deriving (Eq, Show, ToJSON, Parsable)
newtype Command     = Command ST.Text     deriving (Eq, Show, ToJSON, Parsable)
newtype HexCode     = HexCode ST.Text     deriving (Eq, Show, ToJSON, Parsable)
newtype Emoji       = Emoji ST.Text       deriving (Eq, Show, ToJSON, Parsable)

instance Formatted ChannelName where
  formatted (ChannelName x) = "#" <> x

instance Formatted UserName where
  formatted (UserName x) = "@" <> x

instance Formatted Command where
  formatted (Command x) = "/" <> x

instance Formatted HexCode where
  formatted (HexCode x) = "#" <> x

instance Formatted Emoji where
  formatted (Emoji x) = ":" <> x <> ":"
