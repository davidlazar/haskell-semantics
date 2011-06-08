{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.K.FromK
-- Copyright   :  (c) David Lazar, 2011
-- License     :  MIT
--
-- Maintainer  :  lazar6@illinois.edu
-- Stability   :  experimental
-- Portability :  unknown
--
-- Convert K terms to 'Data' values.
-----------------------------------------------------------------------------

module Data.Generics.K.FromK where

import Data.Char (chr)
import Data.Generics
import Text.Printf
import Language.K.Core.Syntax
import Language.Haskell.Exts.Syntax -- Issue 198 (see below)

fromK :: (Data a) => K -> a
fromK = defaultFromK
    `extR` kToInt
    `extR` kToInteger
    `extR` kToBool
    `extR` kToString
    `extR` kToChar
    `extR` kToLiteral -- Issue 198 (see below)
    `extR` kToExp

kToExp :: K -> Exp
kToExp (KApp (KLabel (Syntax "ListExp" : _)) ks) = fromConstrK (toConstr (List [])) ks
kToExp k = defaultFromK k

-- Workaround for Issue 198 in K.
kToLiteral :: K -> Literal
kToLiteral (KApp (KLabel (Syntax "IntLit" : _)) [KApp (KInt i) []]) = Int i
kToLiteral (KApp (KLabel (Syntax "StringLit" : _)) [KApp (KString s) []]) = String s
kToLiteral k = defaultFromK k

kToInt :: K -> Int
kToInt (KApp (KInt i) []) = fromIntegral i

kToInteger :: K -> Integer
kToInteger (KApp (KInt i) []) = i

kToBool :: K -> Bool
kToBool (KApp (KBool b) []) = b

kToString :: K -> String
kToString (KApp (KString s) []) = s

kToChar :: K -> Char
kToChar (KApp (KInt i) []) = chr (fromIntegral i)

defaultFromK :: forall a. (Data a) => K -> a
defaultFromK (KApp (KLabel ((Syntax conStr) : _)) ks) =
    let dataType = dataTypeOf (undefined :: a)
        -- TODO: better error handling:
        con = either error id $ str2con dataType conStr
    in fromConstrK con ks


-- There's probably a better way to do this:
data KS x = KS { ks :: [K] , unKS :: x }

fromConstrK :: (Data a) => Constr -> [K] -> a
fromConstrK c ks = unKS $ gunfold s z c
    where
        s :: forall b r. Data b => KS (b -> r) -> KS r
        s (KS (k:ks) f) = KS ks (f $ fromK k)

        z :: forall r. r -> KS r
        z = KS ks

-- | Turn the given string into a constructor of the requested result type,
-- failing in the monad if the string doesn't represent a constructor of this
-- data type.
str2con :: (Monad m) => DataType -> String -> m Constr
str2con dataType conName =
    case readConstr dataType conName of
        Just con -> return con
        Nothing  -> fail failString
    where failString   = printf formatString (show conName) (show dataType)
          formatString = "Failed to parse %s as a constructor of type %s."
