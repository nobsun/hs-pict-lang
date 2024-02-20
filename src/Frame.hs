-- # Frame フレーム
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Frame
    ( Pos
    , Frame
    , frame
    , unitFrame
    ) where

import Numeric.LinearAlgebra qualified as LA

{- |
フレーム
-}
type Frame = LA.Matrix Double

type Vec = (Double, Double)
type Pos = Vec

frame :: Vec -> Vec -> Vec -> Frame
frame (p,q) (x0, y0) (x1, y1)
    = LA.fromLists [[x0,x1,p],[y0,y1,q],[0,0,1]]

unitFrame :: Frame
unitFrame = frame (0,0) (1,0) (0,1)

