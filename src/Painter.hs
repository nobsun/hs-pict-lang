-- # Painter 描画子
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Painter
    ( Painter
    , blank
    , toPainter
    , transformPainter
    ) where

import Control.Arrow

import Graphics.Gloss qualified as G
import Frame
import Picture

{- | 
描画子
-}
type Painter = Frame -> Picture

blank :: Painter
blank = toPainter []

toPainter :: Picture -> Painter
toPainter p f = map (first (f :)) p

transformPainter :: Pos -> Pos -> Pos -> Painter -> Painter
transformPainter o c0 c1 p f
    = map (first ((f :) . (head (transformer o c0 c1) :))) (p unitFrame)

