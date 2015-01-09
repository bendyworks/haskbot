module Web.Haskbot.Outgoing where

import Web.Haskbot.Newtypes

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
