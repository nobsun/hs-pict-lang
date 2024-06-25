-- # Path G.Pathの取得
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Inspector.Path
    ( Painter
    , path
    ) where

import Control.Arrow

import Graphics.Gloss qualified as G
import Numeric.LinearAlgebra qualified as LA
import Frame
import Picture
import Painter

import GHC.Float

path :: Float -> Painter -> G.Path
path s p = case p unitFrame of
    [(m,gp)] -> case gp of
        G.Line    ps -> sc $ modify m ps
        G.Polygon ps -> sc $ modify m ps
        _            -> []
    _           -> []
    where
        sc = map ((s *) *** (s *))

modifyPath :: Picture -> Picture
modifyPath = map morph
    where
        morph (m, gp) = case m of
            [] -> (m, gp)
            _  -> case gp of
                G.Line ps     -> ([], G.line $ modify m ps)
                G.Polygon ps  -> ([], G.polygon $ modify m ps)
                G.Color c pic -> case morph (m, pic) of
                    ([],pic')      -> ([], G.color c pic')
                    _              -> (m, gp)
                _             -> (m, gp)

modify :: Transformer -> G.Path -> G.Path
modify m
    = map lav2gv
    . LA.toColumns
    . (mconcat m <>)
    . LA.fromColumns
    . map gv2lav

gv2lav :: G.Vector -> LA.Vector Double
gv2lav (x,y) = LA.fromList [float2Double x, float2Double y, 1]

lav2gv :: LA.Vector Double -> G.Vector
lav2gv v = case LA.toList v of
    [x,y,_] -> (double2Float x, double2Float y)
    _       -> error "path.psi: unexpected dimension"

