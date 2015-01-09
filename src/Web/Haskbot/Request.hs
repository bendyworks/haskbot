module Web.Haskbot.Request where

import Web.Scotty.Trans

import Control.Applicative ((<$>))
import qualified Data.Text.Lazy as LT

data RequestError =
    MissingParam LT.Text
  | MalformedParam LT.Text
  | ServerError LT.Text
  deriving (Eq, Show)

instance ScottyError RequestError where
  stringError = ServerError . LT.pack
  showError   = LT.pack . show

type RequestM = ActionT RequestError IO

optParam :: (Parsable a) => LT.Text -> RequestM (Maybe a)
optParam key = do
  val <- lookup key <$> params
  case val of
    Just unparsed -> case parseParam unparsed of
      Left _  -> raise $ MalformedParam key
      Right x -> return $ Just x
    _ -> return Nothing

reqParam :: (Parsable a) => LT.Text -> RequestM a
reqParam key = do
    val <- optParam key
    case val of
      Just x -> return x
      _      -> raise $ MissingParam key
