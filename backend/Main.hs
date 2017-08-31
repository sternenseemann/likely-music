module Main where

import Api

import Codec.Midi (buildMidi)
import Codec.ByteString.Builder
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString ())
import Euterpea hiding (app)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Sound.Likely
import System.Random

api :: Proxy LikelyApi
api = Proxy

midiString :: ToMusic1 a => Music a -> ByteString
midiString = toLazyByteString . buildMidi . toMidi . perform

server :: Server LikelyApi
server = genInterpretation :<|> serveDirectoryWebApp "web/dist"
  where genInterpretation :: OutputFormat -> GraphWithParams -> Handler ByteString
        genInterpretation Midi g = do
          randomGen <- liftIO $ getStdGen
          let maxHops      = fromIntegral . pMaxHops . gpParams $ g
              startingNode = pStartingNode . gpParams $ g
              song         = interpretation randomGen (gpGraph g) startingNode
          case song of
            Nothing   -> throwError err500
            Just song -> return . midiString $ takeNotes maxHops song
        genInterpretation _  _ = throwError err500


app :: Application
app = serve api server

main :: IO ()
main = newStdGen >> run 8081 app
