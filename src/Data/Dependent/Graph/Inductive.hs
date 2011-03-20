{-# LANGUAGE GADTs #-}
module Data.Dependent.Graph.Inductive where

import Data.Dependent.Sum
import Data.GADT.Compare

data LAdj node edge dst where
    LAdj :: node src -> [edge src dst] -> LAdj node edge dst

data RAdj node edge src where
    RAdj :: node dst -> [edge src dst] -> RAdj node edge src

data Context node edge where
    Cxt :: [LAdj node edge t]       -- ^ Edges to the added node
        -> node t                   -- ^ ID of the added node
        -> t                        -- ^ label of the added node
        -> [RAdj node edge t]       -- ^ Edges from the added node
        -> Context node edge

newtype Graph node edge = Gr [Context node edge]

type Decomp node edge = (Maybe (Context node edge), Graph node edge)

empty :: Graph node edge
empty = Gr []

isEmpty :: Graph node edge -> Bool
isEmpty (Gr []) = True
isEmpty _ = False

match :: GEq node => node a -> Graph node edge -> Decomp node edge
match node gr@(Gr []) = (Nothing, gr)
match node (Gr (cxt@(Cxt _ nd _ _) : rest)) =
    case geq node nd of
        Just Refl -> (Just cxt, Gr rest)
        Nothing   -> 
            let ~(mbCxt, Gr rest') = match node (Gr rest)
            in (mbCxt, Gr (cxt : rest'))

