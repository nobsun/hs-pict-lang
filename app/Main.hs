{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main where

import System.Environment

import PictureLanguage

main :: IO ()
main = do 
    { let ?setting = defaultSetting
    ; args <- getArgs
    ; render $ case args of
        "triangle":_ -> squareLimitTriangle
        "wave":_     -> squareLimitWave
        _            -> fish'
    }

