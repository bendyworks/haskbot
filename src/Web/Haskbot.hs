module Web.Haskbot where

import Web.Scotty.Trans

import qualified Data.Text.Lazy as LT
import qualified Web.Haskbot.Outgoing as Outgoing
import qualified Web.Haskbot.SlashCommand as SlashCom

type ServerM = ScottyT RequestError IO

haskbot :: [Plugins] -> IO ()
haskbot = scottyT 3000 id id routes

routes :: HaskbotM
routes = defaultHandler catchError >> do
  post "/slashcommand" SlashCom.handleRequest

catchError :: RequestError -> HaskbotM
catchError err = case err of
  (MissingParam p) -> status status422 >> text (LT.concat ["missing parameter: ", p])
  _                -> status status500 >> text "internal server error"
