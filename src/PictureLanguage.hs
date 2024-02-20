-- # PictureLanguage 図形言語
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module PictureLanguage
    ( module Painter
    , module Picture
    , module Frame
    , module Render
    , module Combinator
    , module SquareLimit
    , squareLimitWave
    , squareLimitTriangle
    ) where

import Painter
import Picture
import Frame
import Render
import Combinator
import SquareLimit
import Painter.Wave
import Painter.Triangle

squareLimitWave :: Painter
squareLimitWave
    = squareLimit0 4 wave

squareLimitTriangle :: Painter
squareLimitTriangle
    = squareLimit 3 crossRBRB crossRBRW crossBRBW
                    crossRBBW crossRBWW crossWBRR crossWBRW
