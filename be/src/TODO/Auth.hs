module TODO.Auth
  ( handleAuth
  , authMiddleware
  ) where

import Data.ByteString (ByteString)
import Data.Function
import Network.HTTP.Types.Status
import Network.Wai
import Servant.API
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

authMiddleware :: Middleware
authMiddleware app req respond = do
  case pathInfo req of
    ["api", "auth"] -> app req respond
    _ -> case lookup "x-csrf-token" $ requestHeaders req of
      Just "letmein" -> app req respond
      _ -> respond $ responseBuilder unauthorized401 [] mempty
