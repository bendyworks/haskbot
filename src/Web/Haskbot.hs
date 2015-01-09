{-# LANGUAGE OverloadedStrings #-}

module Web.Haskbot where

import Network.HTTP.Types

import Web.Scotty.Trans
import Web.Haskbot.Request
import Web.Haskbot.Plugin
import Web.Haskbot.Types

import qualified Data.Text.Lazy as LT

type ServerM = ScottyT RequestError IO

haskbot :: IO ()
haskbot = scottyT 3000 id id routes

routes :: ServerM ()
routes = defaultHandler catchError >>
  post "/slashcommand" handler

catchError :: RequestError -> RequestM ()
catchError err = case err of
  (MissingParam p) -> status status422 >> text (LT.concat ["missing parameter: ", p])
  _                -> status status500 >> text "internal server error"

handler :: RequestM ()
handler = do
  -- find plugin via command and verify token
  runPlugin prog
  successful

prog :: HaskBotScript ()
prog = respond $ Incoming (DirectMsg (UserName "woo")) "Hello, World!"
