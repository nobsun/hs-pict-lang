-- # Painter.Wave 手を振る人
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Painter.Wave
    ( wave
    ) where

import Graphics.Gloss qualified as G
import Painter
import Picture

{- | 
手を振る人
-}
wave :: Painter
wave = toPainter $ toPicture 20 20 wavePict
    where
        wavePict :: G.Picture
        wavePict
            = mconcat
            $ map G.line
            [[(0,13),(3,8),(6,12),(7,11),(5,0)]
            ,[(8,0),(10,6),(12,0)]
            ,[(15,0),(12,10),(20,3)]
            ,[(20,7),(15,13),(12,13),(13,17),(12,20)]
            ,[(8,20),(7,17),(8,13),(6,13),(3,12),(0,17)]
            ]
