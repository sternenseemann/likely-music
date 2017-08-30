module Main where

import Api
import Data.ByteString (ByteString ())
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Sound.Likely

api :: Proxy LikelyApi
api = Proxy

server :: Server LikelyApi
server = genInterpretation
  where genInterpretation :: OutputFormat -> GraphWithParams -> Handler ByteString
        genInterpretation Midi g = do
          let hops = pMaxHops . gpParams $ g
          return undefined


app :: Application
app = serve api server

main :: IO ()
main = run 8081 app
