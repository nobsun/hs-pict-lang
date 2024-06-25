-- # Picture
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Picture
    ( Picture
    , Transformer
    , transformer
    , toPicture
    , fromPicture
    , fromTransformer
    ) where

import Control.Arrow
import Data.Bool
import GHC.Float

import Graphics.Gloss qualified as G
import Numeric.LinearAlgebra qualified as LA

import Frame

{- |
ピクチャ
-}
type Picture = [(Transformer, G.Picture)]

toPicture :: Double -> Double -> G.Picture -> Picture
toPicture sx sy gp
    = [(trans, gp)]
    where
        trans = [LA.fromLists [[recip sx,0,0],[0,recip sy,0],[0,0,1]]]

fromPicture :: Picture -> G.Picture
fromPicture = \ case
    []       -> G.blank
    [(t,gp)] -> fromTransformer t gp
    ps       -> G.pictures [ fromTransformer t gp | (t,gp) <- ps ]

{- | Transformer
-}
type Transformer = [LA.Matrix Double]

transformer :: Pos -> Pos -> Pos -> Transformer
transformer (ox,oy) (c0x,c0y) (c1x,c1y)
    = [frame (ox,oy) (c0x - ox, c0y - oy) (c1x - ox, c1y - oy)]

fromTransformer :: Transformer -> (G.Picture -> G.Picture)
fromTransformer trans = case map (map double2Float) $ LA.toLists m of
    [[x0,x1,_],[y0,y1,_],_]
        | x1 == 0 && y0 == 0
            -> bool (G.translate p' q') id (p'  == 0 && q'  == 0)
             . bool (G.scale x0 y1)     id (x0  == 1 && y1  == 1)
        | otherwise
            -> bool (G.translate p' q') id (p'  == 0 && q'  == 0)
             . bool (G.rotate r0')      id (r0' == 0)
             . bool (G.scale s0' s1')   id (s0' == 1 && s1' == 1)
             . bool (G.rotate r1')      id (r1' == 0)
        where
            p  = m LA.! 0 LA.! 2
            q  = m LA.! 1 LA.! 2
            p' = bool (double2Float p) 0 (abs p < 1.0e-6)
            q' = bool (double2Float q) 0 (abs q < 1.0e-6)
            (u,s,v) = usv where usv = LA.svd (LA.subMatrix (0,0) (2,2) m)
            s'' = uf <> LA.diag s <> vf
            s'  = LA.takeDiag s''
            s0  = s' LA.! 0
            s1  = s' LA.! 1
            s0' = bool (double2Float s0) 1 (abs (s0 - 1) < 1.0e-6)
            s1' = bool (double2Float s1) 1 (abs (s1 - 1) < 1.0e-6)
            r0  = signum (ur LA.! 0 LA.! 1) * acos (ur LA.! 0 LA.! 0) * 180 / pi
            r1  = signum (vr LA.! 1 LA.! 0) * acos (vr LA.! 0 LA.! 0) * 180 / pi
            r0' = bool (double2Float r0) 0 (abs r0 < 1.0e-6)
            r1' = bool (double2Float r1) 0 (abs r1 < 1.0e-6)
            (ur,uf) = dU where dU = decompU u
            (vf,vr) = dV where dV = decompV v

    _       -> error "fromTransformer: unexpected shape"
    where
        m = mconcat trans

decompU, decompV :: LA.Matrix LA.R -> (LA.Matrix LA.R, LA.Matrix LA.R)
decompU u = case LA.cmap signum u of
    s | s LA.! 0 LA.! 0 /= 0 -> (u <> LA.inv f, f)
      | otherwise            -> (LA.fromLists [[0,-1],[1,0]]
                                ,LA.fromLists [[s LA.! 1 LA.! 0,0]
                                              ,[0,negate $ s LA.! 0 LA.! 1]])
        where
            f = LA.diag (LA.takeDiag s)

decompV v = case decompU v of
    (vr, vf) -> (vf, vr)

