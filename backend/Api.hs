--  Copyright 2017 Lukas Epple
--
--  This file is part of likely music.
--
--  likely music is free software: you can redistribute it and/or modify
--  it under the terms of the GNU Affero General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  likely music is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU Affero General Public License for more details.
--
--  You should have received a copy of the GNU Affero General Public License
--  along with likely music. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import Data.Aeson
import Data.ByteString.Lazy (ByteString ())
import Data.Monoid ((<>))
import Data.Ratio
import Data.Text (Text ())
import GHC.Generics
import Servant.API
import Sound.Likely

type LikelyApi = "interpretation" :> Capture "format" OutputFormat 
                                  :> ReqBody '[JSON] GraphWithParams
                                  :> Post '[OctetStream] ByteString
                 :<|> "seed" :> Get '[JSON] Int
                 :<|> Raw

data OutputFormat = Midi | Wav
  deriving (Show, Eq, Ord)

instance FromHttpApiData OutputFormat where
  parseUrlPiece "mid" = Right Midi
  parseUrlPiece "wav" = Right Wav
  parseUrlPiece x     = Left $ "Couldn't match " <> x <> " with {mid, wav}"

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
  { pMaxHops      :: Int
  , pStartingNode :: Node
  , pSeed         :: Int
  } deriving (Show, Eq, Ord)

instance FromJSON Params where
  parseJSON = withObject "Params" $ \v ->
    Params <$> v .: "maxhops"
           <*> v .: "starting_node"
           <*> v .: "seed"
