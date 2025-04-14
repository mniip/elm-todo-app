module TODO.Auth
  ( handleAuth
  , authHandler
  ) where

import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.Function
import Network.Wai
import Servant.API
import Servant.Server
import Servant.Server.Experimental.Auth
import Web.Cookie


sessionCookieName :: ByteString
sessionCookieName = "session"

handleAuth
  :: Monad m
  => m (Headers
    [Header "Set-Cookie" SetCookie, Header "Location" String]
    NoContent)
handleAuth = pure $ NoContent
  & addHeader @"Location" "/"
  & addHeader @"Set-Cookie" defaultSetCookie
    { setCookieName = sessionCookieName
    , setCookieValue = "letmein"
    , setCookiePath = Just "/"
    }

data CSRFAuthorized = CSRFAuthorized

type instance AuthServerData (AuthProtect "csrf") = CSRFAuthorized

authHandler :: AuthHandler Request CSRFAuthorized
authHandler = mkAuthHandler \req
  -> case lookup "x-csrf-token" $ requestHeaders req of
    Just "letmein" -> pure CSRFAuthorized
    _ -> throwError err401
