-- # Inspector.Triangulation 三角形分解
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Inspector.Triangulation
    ( triangulate
    ) where

import Control.Arrow
import Data.List
import Data.Ord
import Graphics.Gloss qualified as G
import Numeric.LinearAlgebra qualified as LA
import Frame
import Picture
import Painter

triangulate :: G.Path -> [G.Path]
triangulate ps
    = iter (length ps) [] ps (reverse ps)
    where
        iter 0 ts _ _ = ts
        iter n ts (p:q:qs) (r:rs) = undefined


rearrange :: G.Path -> G.Path
rearrange ps
    = take n 
    $ maximumBy (comparing (fst . head))
    $ take n
    $ tails
    $ cycle ps
    where
        n = length ps

