-- # Painter.Fish さかな
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Painter.Fish
    ( fish
    , fish'
    ) where

import GHC.Float
import Graphics.Gloss qualified as G
import Numeric.LinearAlgebra qualified as LA
import Combinator
import Painter
import Picture
import Frame
import Render
import Inspector.Path
import Inspector.Grid
import Inspector.Dots

{- |
さかなパス（単純版）
-}
fishPath :: G.Path
fishPath = mconcat fishPaths

{- | 
プリミティブパス
-}
fishLinePrim :: G.Path
fishLinePrim = [(0,0),(20,20),(38,12),(44,4),(60,4),(72,2),(80,0)]

{- |
プリミティブパス（左90度回転逆順）
-}
fishLinePrim' :: G.Path
fishLinePrim' = reverse $ map rr fishLinePrim
    where
        rr (x,y) = (-y,x)

{- |
   ・
   ｜＼(2)
(3)｜  ・
   ｜    ＼(1)
   ・------・
       (0)
-}

fishLine0 :: Painter
fishLine0 = toPainter $ toPicture 80 80 $ G.line fishLinePrim

fishLine1 :: Painter
fishLine1 = rotPi fishLine2

fishLine2 :: Painter
fishLine2 = flipH (rotSqrt2 fishLine0)

fishLine3 :: Painter
fishLine3 = toPainter $ toPicture 80 80 $ G.line fishLinePrim'

fishPaths :: [G.Path]
fishPaths
    = [ fishLinePrim
      , tail $ reverse $ path 80 fishLine1
      , tail $ path 80 fishLine2
      , init $ tail fishLinePrim'
      ]

{- |
さかなパス（凹みあり版）
-}
fishPath' :: G.Path
fishPath'
    = [(0.0,0.0),(20.0,20.0),(38.0,12.0),(44.0,4.0),(60.0,4.0),(72.0,2.0),(80.0,0.0)
      ,(75.0,3.0),(68.0,8.0),(60.0,16.0),(53.0,15.0),(40.0,20.0),(32.0,32.0) -- 凹点
      ,(40.0,40.0),(40.0,60.0),(27.0,65.0),(20.0,64.0),(12.0,72.0),(5.0,77.0)
      ,(0.0,80.0),(-2.0,72.0),(-4.0,60.0),(-4.0,44.0),(-12.0,38.0),(-20.0,20.0)
      ]

fish :: Painter
fish = toPainter $ toPicture 80 80 $ G.line fishPath <> G.color (G.makeColor 1 0.7 0.7 0.5) (G.polygon (init fishPath))

fish' :: Painter
fish' = (<> grid 16 16) . dots 80 . toPainter $ toPicture 80 80 $ G.line fishPath'

init80 :: G.Path -> Painter
init80 = (<> grid 16 16) . toPainter . toPicture 80 80 . G.color (G.makeColor 1 0.7 0.7 0.5) . G.polygon

