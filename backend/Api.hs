{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import Data.Aeson
import Data.ByteString (ByteString ())
import Data.Monoid ((<>))
import Data.Ratio
import Data.Text (Text ())
import GHC.Generics
import Servant.API
import Sound.Likely

type LikelyApi = "interpretation" :> Capture "format" OutputFormat 
                                  :> ReqBody '[JSON] GraphWithParams
                                  :> Post '[OctetStream] ByteString
data OutputFormat = Midi | Wav
    deriving (Show, Eq, Ord)

instance FromHttpApiData OutputFormat where
    parseUrlPiece "midi" = Right Midi
    parseUrlPiece "wav"  = Right Wav
    parseUrlPiece x      = Left $ "Couldn't match " <> x <> " with {midi, wav}"

data GraphWithParams
  = GraphWithParams
  { gpParams :: Params
  , gpGraph  :: Graph
  } deriving (Show, Eq, Ord)

instance FromJSON GraphWithParams where
    parseJSON = withObject "GraphWithParams" $ \v ->
        GraphWithParams <$> v .: "params"
                        <*> v .: "graph"

data Params
  = Params
  { pMaxHops :: Int
  } deriving (Show, Eq, Ord)

instance FromJSON Params where
    parseJSON = withObject "Params" $ \v ->
        Params <$> v .: "maxhops"
