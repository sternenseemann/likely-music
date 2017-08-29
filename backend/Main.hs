{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser ())
import Data.Foldable
import Data.Ratio
import Data.Text (Text ())
import Euterpea
import Sound.Likely

lookupNode :: Text -> [Object] -> Parser Node
lookupNode id nodes = do
  matches <- filterM (fmap (== id) . (.: "id")) nodes
  case matches of
    [node] -> Node <$> node .: "id" <*> (Prim <$> node .: "music")
    _ -> fail "Couldn't match node by id"

buildMap :: [Object] -> [Object] -> Graph -> Parser Graph
buildMap _ [] m = pure m
buildMap nodes (e:es) m = do
  toId <- e .: "to"
  fromId <- e .: "from"
  edge <- Edge <$> (lookupNode toId nodes) <*> (e .: "prob")
  from <- lookupNode fromId nodes
  buildMap nodes es $ insertEdge from edge m

instance FromJSON Graph where
  parseJSON = withObject "Graph" $ \v -> do
    edges <- v .: "edges"
    nodes <- v .: "nodes" 
    buildMap nodes edges $ Graph mempty

instance FromJSON (Primitive Pitch) where
  parseJSON = withObject "Primitive" $ \v -> do
    durObj <- v .: "dur"
    duration <- (%) <$> durObj .: "num" <*> durObj .: "den"
    octave <- v .: "octave"
    pitchClass <- v .: "pitch"
    case pitchClass of
      "Rest" -> pure $ Rest duration
      p -> pure $ Note duration (read pitchClass, octave)

main = return ()
