{-# LANGUAGE GADTs, RankNTypes, UndecidableInstances #-}
module Data.Dependent.Graph where

import Control.Monad
import qualified Data.Dependent.Map as M
import Data.Dependent.Sum
import Data.GADT.Compare
import Data.GADT.Show
import Data.Maybe
import qualified Data.Set as S

data NodeID obj where
    NodeID :: !(obj t) -> NodeID obj

instance GShow obj => Show (NodeID obj) where
    showsPrec p (NodeID tag) = showParen (p>10)
        ( showString "NodeID "
        . gshowsPrec 11 tag
        )

instance GEq obj => Eq (NodeID obj) where
    NodeID x == NodeID y = case geq x y of
        Just Refl -> True; _ -> False

instance GCompare obj => Ord (NodeID obj) where
    compare (NodeID x) (NodeID y) = case gcompare x y of
        GGT -> GT; GEQ -> EQ; GLT -> LT

-- data EdgeID obj where
--     EdgeID :: !(obj src) -> !(obj dst) -> EdgeID obj

data EdgeTag f obj arr t where
    EdgeTag :: !(obj src) -> !(obj dst) -> EdgeTag f obj arr (f (arr src dst))

instance GShow obj => GShow (EdgeTag f obj arr) where
    gshowsPrec p (EdgeTag src dst) = showParen (p>10)
        ( showString "EdgeTag "
        . gshowsPrec 11 src
        . showChar ' '
        . gshowsPrec 11 dst
        )

instance GShow obj => Show (EdgeTag f obj arr t) where
    showsPrec = gshowsPrec

instance GEq obj => GEq (EdgeTag f obj arr) where
    geq (EdgeTag src1 dst1) (EdgeTag src2 dst2) = do
        Refl <- geq src1 src2
        Refl <- geq dst1 dst2
        return Refl

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

mkGraph :: GCompare node
        => [NodeID node] -> [DSum (EdgeTag [] node edge)] -> Graph node edge
mkGraph ns es = Graph
    { nodes = S.fromList ns
    , edges = M.fromList es
    }

lookup :: GCompare node => node src -> node dst -> Graph node edge -> Maybe [edge src dst]
lookup src dst (Graph nodes edges) = do
    guard (NodeID src `S.member` nodes)
    guard (NodeID dst `S.member` nodes)
    M.lookup (EdgeTag src dst) edges
    
