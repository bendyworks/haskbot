{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Web.Haskbot.Newtypes
( Token
, TeamID
, ChannelID
, UserID
, Timestamp
, TriggerWord
, ChannelName
, UserName
, Command
, HexCode
, Emoji
) where

import Data.Aeson (ToJSON)
import Data.Text (Text)

newtype Token       = Token Text       deriving (Eq, Show, ToJSON)
newtype TeamID      = TeamID Text      deriving (Eq, Show, ToJSON)
newtype ChannelID   = ChannelID Text   deriving (Eq, Show, ToJSON)
newtype UserID      = UserID Text      deriving (Eq, Show, ToJSON)
newtype Timestamp   = Timestamp Text   deriving (Eq, Show, ToJSON)
newtype TriggerWord = TriggerWord Text deriving (Eq, Show, ToJSON)
newtype ChannelName = ChannelName Text deriving (Eq, Show, ToJSON)
newtype UserName    = UserName Text    deriving (Eq, Show, ToJSON)
newtype Command     = Command Text     deriving (Eq, Show, ToJSON)
newtype HexCode     = HexCode Text     deriving (Eq, Show, ToJSON)
newtype Emoji       = Emoji Text       deriving (Eq, Show, ToJSON)
