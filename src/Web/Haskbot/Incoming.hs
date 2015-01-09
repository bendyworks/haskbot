{-# LANGUAGE OverloadedStrings #-}

module Web.Haskbot.Incoming where

import Data.Aeson (ToJSON, (.=), toJSON)
import Web.Haskbot.Newtypes

import qualified Data.Text as ST

data Incoming = Incoming
  { inc_channel  :: !ResponseAddr
  , inc_text     :: !ST.Text
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
