-- # SquareLimit 方形の極限
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module SquareLimit
    ( splits0
    , rightSplits0
    , upSplits0
    , cornerSplits0
    , squareLimit0
    , rightSplits
    , upSplits
    , cornerSplits
    , squareLimit
    ) where

import Painter
import Combinator

{- | 辺分割 SICP版
-}
splits0 :: (Painter -> Painter -> Painter)
       -> (Painter -> Painter -> Painter)
       -> Int -> Painter -> Painter
splits0 o0 o1 o p = case o of
    0   -> p
    n+1 -> p `o0` (q `o1` q)
        where
            q = splits0 o0 o1 n p
    _   -> error "splits: negative order"

{- | 右辺分割 SICP版
-}
rightSplits0 :: Int -> Painter -> Painter
rightSplits0 = splits0 (<->) (</>)

{- | 上辺分割 SICP版
-}
upSplits0 :: Int -> Painter -> Painter
upSplits0 = splits0 (flip (</>)) (<->)

{- | 右上角分割 SICP版 -}
cornerSplits0 :: Int -> Painter -> Painter
cornerSplits0 o p = case o of
    0   -> p
    n+1 -> quartet (upSplits0 n p) (cornerSplits0 n p)
                   p               (rightSplits0 n p)
    _   -> error "cornerSplits: negative order"

squareLimit0 :: Int -> Painter -> Painter
squareLimit0 o p
    = quartet nw ne sw se
    where
        ne = cornerSplits0 o p
        nw = flipH ne
        se = flipV ne
        sw = rotPi ne

{- | 辺分割
-}
splits :: (Painter -> Painter -> Painter)
       -> (Painter -> Painter -> Painter)
       -> Int -> Painter -> Painter
       -> Painter
splits o0 o1 o p q = case o of
    0   -> blank
    n+1 -> (p `o1` q) `o0` (r `o1` r)
        where
            r = splits o0 o1 n p q
    _   -> error "splits2: negative order"

{- | 右辺分割
-}
rightSplits :: Int -> Painter -> Painter -> Painter
rightSplits = splits (<->) (</>)

{- | 上辺分割 2
-}
upSplits :: Int -> Painter -> Painter -> Painter
upSplits = splits (flip (</>)) (<->)

{- | 右上角分割
-}
cornerSplits :: Int -> Painter
             -> Painter -> Painter 
             -> Painter -> Painter
             -> Painter
cornerSplits m o p q r s = case m of
    0   -> blank
    n+1 -> quartet (upSplits n p q) (cornerSplits n o p q r s)
                   o                (rightSplits n r s)
    _   -> error "cornerSplits: negative order"

squareLimit :: Int 
            -> Painter -> Painter -> Painter
            -> Painter -> Painter
            -> Painter -> Painter
            -> Painter
squareLimit o c0 c1 c2 l1 l2 l3 l4
    = nonet tl tc tr ml mc mr bl bc br
    where
        mc = c0
        tr = cornerSplits o c1 l1 l2 l3 l4
        bl = rotPi tr
        mr = rightSplits o l3 l4
        ml = rotPi mr
        tc = upSplits o l1 l2
        bc = rotPi tc
        tl = rotL (cornerSplits o c2 (rotL l3) (rotL l4) (rotR l1) (rotR l2))
        br = rotPi tl


