{-# LANGUAGE OverloadedStrings #-}
module Main where

import Api

import Codec.Midi (buildMidi)
import Codec.ByteString.Builder
import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString ())
import qualified Data.ByteString.Lazy as B
import Euterpea hiding (app)
import GHC.IO.Handle
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Sound.Likely
import System.Directory
import System.Exit
import System.Environment
import System.FilePath.Posix
import System.IO
import System.Process
import System.Random

api :: Proxy LikelyApi
api = Proxy

midiString :: ToMusic1 a => Music a -> ByteString
midiString = toLazyByteString . buildMidi . toMidi . perform

server :: Server LikelyApi
server = genInterpretation :<|> randomSeed :<|> serveDirectoryWebApp "web/dist"

randomSeed :: Handler Int
randomSeed = liftIO newStdGen >>= return . fst . random

genInterpretation :: OutputFormat -> GraphWithParams -> Handler ByteString
genInterpretation Midi g = do
  let params       = gpParams g
      maxHops      = fromIntegral . pMaxHops $ params
      randomGen    = mkStdGen $ pSeed params
      song         = interpretation randomGen (gpGraph g) (pStartingNode params)
  return . midiString $ takeNotes maxHops song
genInterpretation Wav g = genInterpretation Midi g >>= synthWav

synthWav :: ByteString -> Handler ByteString
synthWav midi = do
  inName <- tempFile "mid"
  liftIO $ B.writeFile inName midi
  outName <- tempFile "wav"
  (_, _, _, ph) <- liftIO $
    createProcess_ "fluidsynth"
      (proc "fluidsynth"
        [ "-a", "file"
        , "-F", outName
        , "-i"
        , "/usr/share/sounds/sf2/FluidR3_GM.sf2"
        , inName ])
        { std_in = CreatePipe }
  code <- liftIO $ waitForProcess ph
  case code of
    ExitFailure _ -> throwError err500 { errBody = "fluidsynth failed" }
    ExitSuccess -> do
      out <- liftIO $ B.readFile outName
      liftIO $ removePathForcibly outName
      return out

tempFile :: String -> Handler FilePath
tempFile ext = try 0
  where maxtries = 100
        try :: Integer -> Handler FilePath
        try n
          | n < maxtries = do
            progName <- liftIO $ getProgName
            let path = "/tmp" </> addExtension (makeValid progName ++ "-" ++ show n) ext
            exists <- liftIO $ doesFileExist path
            if exists
              then try (n + 1)
              else pure path
          | otherwise = throwError err500
app :: Application
app = serve api server

main :: IO ()
main = newStdGen >> run 8081 app
