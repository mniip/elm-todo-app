module Main where

import Data.Proxy
import Network.Wai.Handler.Warp
import Servant.Server
import TODO.API
import TODO.Auth
import TODO.Handlers
import TODO.Store.InMemory

main :: IO ()
main = do
  ref <- newInMemoryStore
  run 8080 $ authMiddleware $ serveWithContextT
    (Proxy @API)
    EmptyContext
    (runInMemoryStoreT ref)
    handlers
