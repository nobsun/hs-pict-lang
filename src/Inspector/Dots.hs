-- # Inspector.Grid グリッド
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Inspector.Dots
    ( dots
    ) where

import GHC.Float
import Graphics.Gloss qualified as G
import Painter
import Picture
import Frame
import Inspector.Path
{- | 
ドット
-}
dot :: Float -> G.Vector -> G.Picture
dot s (x,y) = G.translate x y (G.circleSolid (s / 200))

dots :: Float -> Painter -> Painter
dots s = (<>) <*> toPainter . toPicture s' s' . G.pictures . map (dot s) . path s
    where
        s' = float2Double s
