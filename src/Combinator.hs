-- # Combinator 結合子
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Combinator
    ( flipH
    , flipV
    , rotL
    , rotR
    , rotPi
    , rotSqrt2
    , rotSqrt2'
    , beside
    , above
    , (<->)
    , (</>)
    , twinH
    , twinV
    , quartet
    , nonet
    ) where

import Painter

{- | 
左右反転
-}
flipH :: Painter -> Painter
flipH = transformPainter (1,0) (0,0) (1,1)

{- |
上下反転
-}
flipV :: Painter -> Painter
flipV = transformPainter (0,1) (1,1) (0,0)

{- |
左90度回転
-}
rotL :: Painter -> Painter
rotL = transformPainter (1,0) (1,1) (0,0)

{- |
右90度回転
-}
rotR :: Painter -> Painter
rotR = transformPainter (0,1) (0,0) (1,1)

{- |
180度回転
-}
rotPi :: Painter -> Painter
rotPi = flipH . flipV

{- |
左上中心に45度回転1/√2スケール
-}
rotSqrt2 :: Painter -> Painter
rotSqrt2 = transformPainter (0.5,0.5) (1,1) (0,1)

{- |
右下中心に45度回転1/√2スケール
-}
rotSqrt2' :: Painter -> Painter
rotSqrt2' = transformPainter (0.5,0.5) (1,0) (1,1)

{- |
横並び一般
-}
beside :: Double -> Double -> Painter -> Painter -> Painter
beside m n p q
    =  transformPainter (0,0) (r,0) (0,1) p
    <> transformPainter (r,0) (1,0) (r,1) q
    where
        r = m / (m + n)

{- |
縦並び一般
-}
above :: Double -> Double -> Painter -> Painter -> Painter
above m n p q
    =  transformPainter (0,r) (1,r) (0,1) p
    <> transformPainter (0,0) (1,0) (0,r) q
    where
        r = n / (m + n)

{- | 横並び等幅
-}
(<->) :: Painter -> Painter -> Painter 
(<->) = beside 1 1

{- | 縦並び等高
-}
(</>) :: Painter -> Painter -> Painter
(</>) = above 1 1

{- | 双子横
-}
twinH :: Painter -> Painter
twinH p = p <-> p

{- | 双子縦
-}
twinV :: Painter -> Painter
twinV p = p </> p

{- | カルテット
-}
quartet :: Painter -> Painter
        -> Painter -> Painter
        -> Painter
quartet tl tr bl br
    = (tl <-> tr) </> (bl <-> br)

{- | ノネット
-}
nonet :: Painter -> Painter -> Painter
      -> Painter -> Painter -> Painter
      -> Painter -> Painter -> Painter
      -> Painter
nonet tl tc tr ml mc mr bl bc br
    = above 1 2 
        (beside 1 2 tl (tc <-> tr))
        (beside 1 2 ml (mc <-> mr)
         </> beside 1 2 bl (bc <-> br))

{- | 辺分割
-}
splits :: (Painter -> Painter -> Painter)
       -> (Painter -> Painter -> Painter)
       -> Int -> Painter -> Painter
splits o0 o1 o p = case o of
    0   -> p
    n+1 -> p `o0` (q `o1` q)
        where
            q = splits o0 o1 n p
    _   -> error "splits: negative order"

{- | 右辺分割
-}
rightSplits :: Int -> Painter -> Painter
rightSplits = splits (<->) (</>)

{- | 上辺分割
-}
upSplits :: Int -> Painter -> Painter
upSplits = splits (flip (</>)) (<->)

{- | 右上角分割 -}
cornerSplits :: Int -> Painter -> Painter
cornerSplits o p = case o of
    0   -> p
    n+1 -> quartet (upSplits n p) (cornerSplits n p)
                   p               (rightSplits n p)
    _   -> error "cornerSplits: negative order"

{- | 辺分割 2
-}
splits2 :: (Painter -> Painter -> Painter)
        -> (Painter -> Painter -> Painter)
        -> Int -> Painter -> Painter
        -> Painter
splits2 o0 o1 o p q = case o of
    0   -> blank
    n+1 -> (p `o1` q) `o0` (r `o1` r)
        where
            r = splits2 o0 o1 n p q
    _   -> error "splits2: negative order"

{- | 右辺分割 2
-}
rightSplits2 :: Int -> Painter -> Painter -> Painter
rightSplits2 = splits2 (<->) (</>)

{- | 上辺分割 2
-}
upSplits2 :: Int -> Painter -> Painter -> Painter
upSplits2 = splits2 (flip (</>)) (<->)

{- | 右上角分割
-}
cornerSplits2 :: Int -> Painter
              -> Painter -> Painter 
              -> Painter -> Painter
              -> Painter
cornerSplits2 m o p q r s = case m of
    0   -> blank
    n+1 -> quartet (upSplits2 n p q) (cornerSplits2 n o p q r s)
                   o                 (rightSplits2 n r s)
    _   -> error "cornerSplits: negative order"

