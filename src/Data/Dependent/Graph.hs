{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Dependent.Graph where

import Control.Monad
import qualified Data.Dependent.Map as M
import Data.GADT.Compare
import Data.Maybe
import qualified Data.Set as S

data NodeID obj where
    NodeID :: !(obj t) -> NodeID obj

instance GCompare obj => Eq (NodeID obj) where
    NodeID x == NodeID y = case gcompare x y of
        GEQ -> True; _ -> False

instance GCompare obj => Ord (NodeID obj) where
    compare (NodeID x) (NodeID y) = case gcompare x y of
        GGT -> GT; GEQ -> EQ; GLT -> LT

data EdgeID obj where
    EdgeID :: !(obj src) -> !(obj dst) -> EdgeID obj

data EdgeTag f obj arr t where
    EdgeTag :: !(obj src) -> !(obj dst) -> EdgeTag f obj arr (f (arr src dst))

instance GCompare obj => GCompare (EdgeTag f obj arr) where
    gcompare (EdgeTag src1 dst1) (EdgeTag src2 dst2) = 
        case gcompare src1 src2 of
            GLT -> GLT
            GGT -> GGT
            GEQ -> case gcompare dst1 dst2 of
                GLT -> GLT
                GGT -> GGT
                GEQ -> GEQ

data Graph node edge = Graph 
    { nodes :: !(S.Set (NodeID node))
    , edges :: !(M.DMap (EdgeTag [] node edge))
    }

empty :: Graph node edge
empty = Graph S.empty M.empty

singleton :: node t -> Graph node edge
singleton obj = Graph (S.singleton (NodeID obj)) M.empty

lookup :: GCompare node => node src -> node dst -> Graph node edge -> Maybe [edge src dst]
lookup src dst (Graph nodes edges) = do
    guard (NodeID src `S.member` nodes)
    guard (NodeID dst `S.member` nodes)
    M.lookup (EdgeTag src dst) edges
    
