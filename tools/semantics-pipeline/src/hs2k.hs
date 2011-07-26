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
-- Command-line tool for translating Haskell programs to K terms.
-----------------------------------------------------------------------------

module Main where

import Data.Generics.K
import Language.Haskell.Exts (fromParseResult, parseFileContents)
import Language.K.Core.Pretty.KMode

main :: IO ()
main = interact ((++ "\n") . prettyPrint . toK . fromParseResult . parseFileContents)
