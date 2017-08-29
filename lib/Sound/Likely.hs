{-# LANGUAGE OverloadedStrings #-}
module Sound.Likely where

import Data.Maybe
import Data.Text (Text ())
import Euterpea
import System.Random
import qualified Data.Map as M
import qualified Data.Set as S

type Probability = Double

data Node
  = Node
  { nId :: Text
  , nMusic :: Music Pitch
  } deriving (Show, Eq, Ord)

data Edge
  = Edge
  { eTo   :: Node
  , eProb :: Probability
  } deriving (Show, Eq, Ord)

newtype Graph = Graph { unGraph :: M.Map Node (S.Set Edge) }

insertNode :: Node -> Graph -> Graph
insertNode t = Graph . M.insertWith S.union t S.empty . unGraph

insertEdge :: Node -> Edge -> Graph -> Graph
insertEdge n e =
  insertNode n . Graph . M.insertWith S.union n (S.singleton e) . unGraph

interpretation :: RandomGen g => g -> Graph -> Node -> Maybe (Music Pitch)
interpretation gen graph n = (:+:)
  <$> Just (nMusic n)
  <*> fmap recurse (M.lookup n (unGraph graph))
  where (prob, gen') = randomR (0.0, 1.0) gen
        recurse edges = if S.null edges
                          then empty
                          else fromMaybe empty . interpretation gen graph .
                            eTo . edgeForRoll prob $ edges

edgeForRoll :: Probability -> S.Set Edge -> Edge
edgeForRoll prob set =
  let curr = S.elemAt 0 set
    in if prob <= eProb curr
         then curr
         else edgeForRoll (prob - eProb curr) (S.delete curr set)

empty :: Music a
empty = Prim (Rest 0)

exampleGraph :: Graph
exampleGraph = Graph $ M.fromList
  [ (Node "bla" (c 4 qn), S.fromList [ Edge (Node "blub" (d 4 qn)) 1 ] )
  , (Node "blub" (d 4 qn), S.fromList [ ])
  ]
