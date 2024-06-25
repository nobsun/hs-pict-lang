-- # Render
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Render
    ( Setting
    , defaultSetting
    , render
    , rendering
    ) where

import Graphics.Gloss qualified as G
import Painter
import Picture
import Frame

{- | 
描画セッティング
-}
data Setting
    = Setting
    { window  :: G.Display
    , bgcolor :: G.Color
    , framing :: Frame
    }

{- |
描画セッティングの既定値
-}
defaultSetting :: Setting
defaultSetting = Setting
    { window  = G.FullScreen
    , bgcolor = G.white
    , framing = frame (-512,-512) (1024,0) (0,1024)
    }

setting1600 :: Setting
setting1600 = Setting
    { window = G.InWindow "1600 x 1280" (1600,1280) (100,100)
    , bgcolor = G.white
    , framing = frame (-512,-512) (1024,0) (0,1024)
    }

{- |
描画子による描画
-}
render :: (?setting :: Setting) => Painter -> IO ()
render p
    = G.display ?setting.window ?setting.bgcolor 
    $ fromPicture
    $ p ?setting.framing

rendering :: Painter -> IO ()
rendering p = do
    { let ?setting = setting1600
    ; render p
    }
