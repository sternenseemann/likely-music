{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Sound.Likely
  ( Probability
  , ID
  , Node (..)
  , Edge (..)
  , Graph (..)
  , insertNode
  , insertEdge
  , interpretation
  , takeNotes
  , emptyMusic
  , exampleGraph
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser ())
import Data.Maybe
import Data.Text (Text ())
import Euterpea
import System.Random
import qualified Data.Map as M
import qualified Data.Set as S

type Probability = Double
type ID = Text

data Node
  = Node
  { nId :: ID
  , nMusic :: Music Pitch
  } deriving (Show, Eq, Ord)

data Edge
  = Edge
  { eTo   :: Node
  , eProb :: Probability
  } deriving (Show, Eq, Ord)

newtype Graph = Graph { unGraph :: M.Map Node (S.Set Edge) }
  deriving (Show, Eq, Ord)

insertNode :: Node -> Graph -> Graph
insertNode t = Graph . M.insertWith S.union t S.empty . unGraph

insertEdge :: Node -> Edge -> Graph -> Graph
insertEdge n e =
  insertNode n . Graph . M.insertWith S.union n (S.singleton e) . unGraph

interpretation :: RandomGen g => g -> Graph -> Node -> Music Pitch
interpretation gen graph n = (nMusic n) :+:
  recurse (fromMaybe S.empty (M.lookup n (unGraph graph)))
  where (prob, gen') = randomR (0.0, 1.0) gen
        recurse edges =
          if S.null edges
            then emptyMusic
            else interpretation gen graph
                 . eTo . edgeForRoll prob $ edges

edgeForRoll :: Probability -> S.Set Edge -> Edge
edgeForRoll prob set =
  let curr = S.elemAt 0 set
    in if prob <= eProb curr
         then curr
         else edgeForRoll (prob - eProb curr) (S.delete curr set)

emptyMusic :: Music a
emptyMusic = Prim (Rest 0)

exampleGraph :: Graph
exampleGraph = Graph $ M.fromList
  [ (Node "bla" (c 4 qn), S.fromList [ Edge (Node "blub" (d 4 qn)) 1 ] )
  , (Node "blub" (d 4 qn), S.fromList [ ])
  ]

-- | Take the first @n@ notes of a 'Music'
takeNotes :: Integer -> Music a -> Music a
takeNotes _ m@(Prim _) = m
takeNotes n (Modify c m) = Modify c $ takeNotes n m
takeNotes _ m@(_ :=: _) = m
takeNotes n (m1 :+: m2)
  | n <  1    = emptyMusic
  | n == 1    = m1
  | otherwise = m1 :+: takeNotes (n - 1) m2

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v ->
    Node <$> v .: "id" <*> (Prim <$> v .: "music")

lookupNode :: Text -> [Object] -> Parser Node
lookupNode id nodes = do
  matches <- filterM (fmap (== id) . (.: "id")) nodes
  case matches of
    [node] -> parseJSON (Object node)
    _ -> fail "Couldn't match node by id"

buildMap :: [Object] -> [Object] -> Graph -> Parser Graph
buildMap _ [] m = pure m
buildMap nodes (e:es) m = do
  toId <- e .: "to"
  fromId <- e .: "from"
  edge <- Edge <$> lookupNode toId nodes <*> e .: "prob"
  from <- lookupNode fromId nodes
  buildMap nodes es $ insertEdge from edge m

instance FromJSON Graph where
  parseJSON = withObject "Graph" $ \v -> do
    edges <- v .: "edges"
    nodes <- v .: "nodes"
    buildMap nodes edges $ Graph mempty

instance FromJSON (Primitive Pitch) where
  parseJSON = withObject "Primitive" $ \v -> do
    -- TODO Ratio _Integer_ is easy DOSable
    -- RAM consumption
    duration <- v .: "dur"
    octave <- v .: "octave"
    pitchClass <- v .: "pitch"
    case pitchClass of
      "Rest" -> pure $ Rest duration
      p -> pure $ Note duration (read pitchClass, octave)
