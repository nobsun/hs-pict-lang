-- # Painter.Triangle 三角形
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Painter.Triangle
    ( triangle
    , cross
    , lambda
    , crossWBRR
    , crossWBRW
    , crossRBBW
    , crossRBWW
    , crossRBRB
    , crossRBRW
    , crossBRBW
    ) where

import GHC.Float
import Graphics.Gloss qualified as G
import Combinator
import Painter
import Picture

{- | 
三角形
-}
triangle :: G.Color -> Painter
triangle c
    = toPainter
    $ toPicture 1 1
    $ G.color c
    $ G.polygon [(0,0),(1,0),(0,1)]

lambda :: Painter
lambda = cross G.green G.green G.red G.blue

cross  :: G.Color -> G.Color -> G.Color -> G.Color -> Painter
cross n e s w 
    = mconcat $ zipWith id (iterate (rotR .) id) (map (rotSqrt2 . triangle) [n,e,s,w])

crossRBBW, crossRBWW, crossWBRR, crossWBRW :: Painter
crossRBBW = cross G.red G.blue G.blue G.white
crossRBWW = cross G.red G.blue G.white G.white
crossWBRR = cross G.white G.blue G.red G.red
crossWBRW = cross G.white G.blue G.red G.white

crossRBRB, crossRBRW, crossBRBW :: Painter
crossRBRB = cross G.red G.blue G.red G.blue
crossRBRW = cross G.red G.blue G.red G.white
crossBRBW = cross G.blue G.red G.blue G.white
