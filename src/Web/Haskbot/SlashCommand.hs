module Web.Haskbot.SlashCommand where

import Web.Haskbot.Newtypes

import qualified Data.Text as ST

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
