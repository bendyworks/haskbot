{-# LANGUAGE DeriveFunctor, OverloadedStrings #-}

module Web.Haskbot.Plugin where

import Control.Applicative ((<*>), (<$>))
import Control.Monad.Free (liftF, Free(..))
import Control.Monad.IO.Class (liftIO)

import Web.Haskbot.Types
import Web.Haskbot.Request

data SlashF next =
    GetSlashCommand (SlashCommand -> next)
  | Respond Incoming next
  deriving (Functor)

type HaskBotScript a = Free SlashF a

fromParams :: RequestM SlashCommand
fromParams = SlashCommand <$> reqParam "token"
                          <*> reqParam "team_id"
                          <*> reqParam "channel_id"
                          <*> reqParam "channel_name"
                          <*> reqParam "user_id"
                          <*> reqParam "user_name"
                          <*> reqParam "command"
                          <*> optParam "text"



getSlashCommand :: HaskBotScript SlashCommand
getSlashCommand = liftF (GetSlashCommand id)

respond :: Incoming -> HaskBotScript ()
respond s = liftF (Respond s ())

runPlugin :: HaskBotScript a -> RequestM ()
runPlugin (Free (GetSlashCommand f)) = fromParams >>= runPlugin . f
runPlugin (Free (Respond incoming nxt)) = liftIO (print incoming) >> runPlugin nxt
runPlugin (Pure _) = return ()
