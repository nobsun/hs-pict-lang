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
module Inspector.Grid
    ( grid
    ) where

import Graphics.Gloss qualified as G
import Painter
import Picture

{- | 
グリッド
-}
grid :: Int -> Int -> Painter
grid r c
    = toPainter $ toPicture s s 
    $ G.color (G.makeColor 0.5 0.5 0.5 0.3)
    $ G.pictures
    $ [G.line [(0,j'), (w,j')] | j <- [0 .. r], let j' = fromIntegral j]
      ++ 
      [G.line [(i',0), (i',h)] | i <- [0 .. c], let i' = fromIntegral i]
    where
        h = fromIntegral r
        w = fromIntegral c
        s = fromIntegral $ max r c

