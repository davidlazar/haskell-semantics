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
import Language.Haskell.Exts.Fresh
import qualified Data.Map as Map

main :: IO ()
main = interact k2hs

k2hs :: String -> String
k2hs = (++ "\n")
     . prettyPrint
     . concretizeFix
     . fromK
     . either (error . show) id
     . parseK

-- | Name of the fresh identifier used for @fix@ in the semantics.
defaultFixIdent :: String
defaultFixIdent = "fix"

-- | Concretize fresh variables, import Data.Function, and add a top-level
-- declaration for the fresh fix identifier.
concretizeFix :: Module -> Module
concretizeFix m = addDecls dfimport (fixDecl ident) m'
    where (m', cft) = concretize' m
          ident = Map.findWithDefault s s cft
          s = defaultFixIdent

-- | Add the given import declaration and top-level declaration to the given
-- module.
addDecls :: ImportDecl -> Decl -> Module -> Module
addDecls importdecl decl
      (Module loc nom pragmas warn exports imports decls)
    = (Module loc nom pragmas warn exports (importdecl : imports) (decl : decls))

-- | fixDecl "foo" -> "foo = Data.Function.fix"
fixDecl :: String -> Decl
fixDecl ident
    = PatBind (SrcLoc "<unknown.hs>" 0 0) (PVar (Ident ident)) Nothing
        (UnGuardedRhs (Var (Qual (ModuleName "Data.Function") (Ident "fix"))))
        (BDecls [])

-- | "import qualified Data.Function"
dfimport :: ImportDecl
dfimport
    = ImportDecl
    { importLoc = SrcLoc "<unknown>.hs" 0 0
    , importModule = ModuleName "Data.Function"
    , importQualified = True
    , importSrc = False
    , importPkg = Nothing
    , importAs = Nothing
    , importSpecs = Nothing
    }
