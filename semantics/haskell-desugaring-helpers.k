kmod HASKELL-DESUGARING-HELPERS is
    including K
    including HASKELL-SYNTAX
    including PL-CONVERSION


    kvar Exp     : Exp
    kvar Pat     : Pat
    kvar Count   : Int
    kvar Fresh   : Int
    kvar Name    : String
    kvar Prefix  : String
    kvar Decls   : HsList
    kvar HsList  : HsList
    kvar Pats    : HsList


    syntax SrcLoc ::= `NoLoc
    macro NoLoc = SrcLoc "<unknown.hs>" 0 0


    syntax Exp ::= bindIfNonEmpty Binds Exp
    rule bindIfNonEmpty (BDecls []) Exp => Exp
        [structural]
    rule bindIfNonEmpty (BDecls Decls) Exp => Let (BDecls Decls) Exp
        if Decls =/=Bool []
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


    syntax Exp ::= listToTuple HsList
    rule listToTuple ((:) Exp []) => Exp
        [structural]
    rule listToTuple ((:) Exp HsList) => Tuple ((:) Exp HsList)
        if nonempty HsList
        [structural]


    syntax Pat ::= listToPTuple HsList
    rule listToPTuple ((:) Pat []) => Pat
        [structural]
    rule listToPTuple ((:) Pat HsList) => PTuple ((:) Pat HsList)
        if nonempty HsList
        [structural]


    syntax Bool ::= nonempty HsList
    rule nonempty [] => false
        [structural]
    rule nonempty ((:) _ _) => true
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
