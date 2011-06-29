kmod HASKELL-DESUGARING-HELPERS is
    including K
    including HASKELL-SYNTAX
    including PL-CONVERSION


    kvar Exp     : Exp
    kvar BExp    : Exp
    kvar Rhs     : Rhs
    kvar Binds   : Binds
    kvar Count   : Int
    kvar Fresh   : Int
    kvar Name    : String
    kvar Prefix  : String
    kvar Decls   : HsList
    kvar GRhss   : HsList
    kvar HsList  : HsList
    kvar Matches : HsList
    kvar Pats    : HsList
    kvar Stmts   : HsList


    syntax SrcLoc ::= `NoLoc
    macro NoLoc = SrcLoc "<unknown.hs>" 0 0


    syntax Exp ::= bindIfNonEmpty Binds Exp
    rule bindIfNonEmpty (BDecls []) Exp => Exp
        [structural]
    rule bindIfNonEmpty (BDecls Decls) Exp => Let (BDecls Decls) Exp
        if Decls =/=Bool []
        [structural]


    syntax Exp ::= trRhsToExp Rhs
    rule trRhsToExp (UnGuardedRhs Exp) => Exp
    rule trRhsToExp (GuardedRhss GRhss) => trGRhssToExp GRhss


    syntax Exp ::= trGRhssToExp HsList
    rule trGRhssToExp [] => App (Var (UnQual (Ident "error"))) (Lit (StringLit "Unmatched pattern"))
    rule trGRhssToExp ((:) (GuardedRhs _ ((:) (Qualifier BExp) []) Exp) GRhss) =>
        If BExp Exp (trGRhssToExp GRhss)


    syntax HsList ::= trMatchesToAlts HsList
    rule trMatchesToAlts [] => []
        [structural]
    rule trMatchesToAlts ((:) (Match _ _ Pats _ Rhs Binds) Matches) =>
        (:) (Alt NoLoc (PTuple Pats) (trRhsToGuardedAlts Rhs) Binds)
            (trMatchesToAlts Matches)
        [structural]


    syntax GuardedAlts ::= trRhsToGuardedAlts Rhs
    rule trRhsToGuardedAlts (UnGuardedRhs Exp) => UnGuardedAlt Exp
        [structural]
    rule trRhsToGuardedAlts (GuardedRhss GRhss) =>
        GuardedAlts (trGRhssToGAlts GRhss)
        [structural]


    syntax HsList ::= trGRhssToGAlts HsList
    rule trGRhssToGAlts [] => []
        [structural]
    rule trGRhssToGAlts ((:) (GuardedRhs _ Stmts Exp) GRhss) =>
        (:) (GuardedAlt NoLoc Stmts Exp) (trGRhssToGAlts GRhss)
        [structural]


    syntax Int ::= getFunctionArity HsList
    rule getFunctionArity ((:) (Match _ _ Pats _ _ _) _) => length Pats
        [structural]


    syntax String ::= getFunctionName HsList
    rule getFunctionName ((:) (Match _ (Ident Name) _ _ _ _) _) => Name
        [structural]


    syntax HsList ::= freshPVarList String Int Int
    rule freshPVarList _ _ 0 => []
        [structural]
    rule freshPVarList Prefix Fresh Count =>
        (:) (PVar (Ident ("@" ++ Prefix ++ Int2String(Fresh))))
            (freshPVarList Prefix (Fresh +Int 1) (Count -Int 1))
        if Count =/=Bool 0
        [structural]


    syntax HsList ::= freshVarList String Int Int
    rule freshVarList _ _ 0 => []
        [structural]
    rule freshVarList Prefix Fresh Count =>
        (:) (Var (UnQual (Ident ("@" ++ Prefix ++ Int2String(Fresh)))))
            (freshVarList Prefix (Fresh +Int 1) (Count -Int 1))
        if Count =/=Bool 0
        [structural]


    syntax Int ::= length HsList
    rule length [] => 0
        [structural]
    rule length ((:) _ HsList) => 1 +Int length HsList
        [structural]


    syntax String ::= String ++ String [gather(E e)]
    rule Str1:String ++ Str2:String => Str1:String +String Str2:String
        [structural]

endkm
