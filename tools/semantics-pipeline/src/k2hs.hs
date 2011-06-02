-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) David Lazar, 2011
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- Command-line tool for translating K terms to Haskell programs.
-----------------------------------------------------------------------------

module Main where

import Data.Generics.K
import Language.K.Core.Parser
import Language.Haskell.Exts

main :: IO ()
main = interact ((++ "\n") . pp . fromK . either (error . show) id . parseK)
    where pp :: Module -> String
          pp = prettyPrint
