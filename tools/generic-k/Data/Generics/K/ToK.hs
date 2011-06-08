-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.K.ToK
-- Copyright   :  (c) David Lazar, 2011
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- Convert 'Data' values to K terms.
-----------------------------------------------------------------------------

module Data.Generics.K.ToK where

import Data.Char (ord)
import Data.Generics
import Language.K.Core.Syntax
import Language.Haskell.Exts.Syntax -- Issue 198 (see below)

toK :: (Data a) => a -> K
toK = defaultToK
    `extQ` stringToK

stringToK :: String -> K
stringToK s = KApp (KString s) []

defaultToK :: (Data a) => a -> K
defaultToK a = KApp (toKLabel a) (gmapQ toK a)

toKLabel :: (Data a) => a -> KLabel
toKLabel = defaultToKLabel
    `extQ` KInt         -- Integer
    `extQ` KBool        -- Bool
    `extQ` intToKLabel  -- Int
    `extQ` charToKLabel -- Char
    `extQ` literalToKLabel -- Issue 198 (see below)

-- Workaround for Issue 198 in K.
literalToKLabel :: Literal -> KLabel
literalToKLabel (Int _) = KLabel [Syntax "IntLit", Hole]
literalToKLabel (String _) = KLabel [Syntax "StringLit", Hole]
literalToKLabel l = defaultToKLabel l

intToKLabel :: Int -> KLabel
intToKLabel = KInt . fromIntegral

charToKLabel :: Char -> KLabel
charToKLabel = KInt . fromIntegral . ord

defaultToKLabel :: (Data a) => a -> KLabel
defaultToKLabel a = KLabel $ Syntax ctor : replicate (glength a) Hole
    where ctor = showConstr . toConstr $ a
