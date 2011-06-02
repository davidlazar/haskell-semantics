-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.K
-- Copyright   :  (c) David Lazar, 2011
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- Convert K terms to and from 'Data' values.
-----------------------------------------------------------------------------

module Data.Generics.K
    ( toK
    , fromK
    ) where

import Data.Generics.K.ToK (toK)
import Data.Generics.K.FromK (fromK)
