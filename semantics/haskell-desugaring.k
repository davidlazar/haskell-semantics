kmod HASKELL-DESUGARING is
    including HASKELL-SYNTAX
    including HASKELL-DESUGARING-HELPERS
    including K
    including PL-CONVERSION

    configuration
        <T multiplicity="?">
            <k> .K </k>
            <fresh> 0 </fresh>
        </T>
        <result multiplicity="?"> .K </result>


    syntax Bag ::= [[ K ]]
    macro [[ K:K ]] = <T_> <k> K:K </k> <_/T>

    syntax KResultLabel ::= desugared
    syntax K ::= desugaredProgram List{K}

    rule <T_> <k> KR:KResult </k> <_/T> =>
           <result> desugaredProgram(eraseKLabel(desugared, KR:KResult)) </result>

    rule desugaredProgram(K:K) => K:K

    kvar E E1 E2 E3 : Exp
    kvar I N : Int

    /*
    The integer literal i is equivalent to fromInteger i, where fromInteger is
    a method in class Num (see Section 6.4.1).
    */

    rule <k> Lit (IntLit I) => desugared(App (Var (UnQual (Ident "fromInteger"))) (Lit (IntLit I))) <_/k>


    /*
    The following identities hold:

    e1 op e2 = (op) e1 e2
    -e       = negate (e)

    */

    rule NegApp E => App (Var (UnQual (Ident "negate"))) E

    rule InfixApp E1 Q:QOp E2 => App (App fromInfix(Q:QOp) E1) E2

    syntax Exp ::= fromInfix QOp
    rule fromInfix(QVarOp QN:QName) => Var QN:QName [structural]
    rule fromInfix(QConOp QN:QName) => Con QN:QName [structural]

    /*
    The following identities hold:

    (op e) = \ x -> x op e
    (e op) = \ x -> e op x

    where op is a binary operator, e is an expression, and x is a variable that
    does not occur free in e.
    */

    rule <k>
             RightSection Q:QOp E =>
               Lambda NoLoc ((:) (PVar (Ident FreshId)) [])
                 (InfixApp (Var (UnQual (Ident FreshId))) Q:QOp E)
         <_/k>
         <fresh> N => N +Int 1 </fresh>
         where FreshId = ("@x" +String Int2String(N))

    rule <k>
             LeftSection E Q:QOp =>
               Lambda NoLoc ((:) (PVar (Ident FreshId)) [])
                 (InfixApp E Q:QOp (Var (UnQual (Ident FreshId))))
         <_/k>
         <fresh> N => N +Int 1 </fresh>
         where FreshId = ("@x" +String Int2String(N))

    /*
    The following identity holds:

    if e1 then e2 else e3 = case e1 of { True -> e2 ; False -> e3 }

    where True and False are the two nullary constructors from the type Bool, as
    defined in the Prelude. The type of e1 must be Bool; e2 and e3 must have the
    same type, which is also the type of the entire conditional expression.
    */

    rule If E E1 E2 => Case E ((:) Alt NoLoc (PApp (UnQual (Ident "True" )) []) (UnGuardedAlt E1) (BDecls [])
                               (:) Alt NoLoc (PApp (UnQual (Ident "False")) []) (UnGuardedAlt E2) (BDecls [])
                               [])


    /*
    (e) is equivalent to e.
    */

    rule Paren E => E


    /*
    Arithmetic sequences satisfy these identities:

    [e1 ..]        = enumFrom e1
    [e1, e2 ..]    = enumFromThen e1 e2
    [e1 .. e3]     = enumFromTo e1 e3
    [e1, e2 .. e3] = enumFromThenTo e1 e2 e3

    where enumFrom, enumFromThen, enumFromTo, and enumFromThenTo are class
    methods in the class Enum as defined in the Prelude (see Figure 6.1).
    */

    rule EnumFrom E1 => App (Var (UnQual (Ident "enumFrom"))) E1
    rule EnumFromThen E1 E2 => App (App (Var (UnQual (Ident "enumFromThen"))) E1) E2
    rule EnumFromTo E1 E3 => App (App (Var (UnQual (Ident "enumFromTo"))) E1) E3
    rule EnumFromThenTo E1 E2 E3 => App (App (App (Var (UnQual (Ident "enumFromThenTo"))) E1) E2) E3

    /*
    List comprehensions satisfy these identities, which may be used as a
    translation into the kernel:

    [ e | True ]         = [e]
    [ e | q ]            = [ e | q, True ]
    [ e | b, Q ]         = if b then [ e | Q ] else []
    [ e | p <- l, Q ]    = let ok p = [ e | Q ]
                               ok _ = []
                           in concatMap ok l
    [ e | let decls, Q ] = let decls in [ e | Q ]

    where e ranges over expressions, p over patterns, l over list-valued
    expressions, b over boolean expressions, decls over declaration lists, q
    over qualifiers, and Q over sequences of qualifiers. ok is a fresh
    variable. The function concatMap, and boolean value True, are defined in
    the Prelude.
    */

    rule ListComp E ((:) (QualStmt (Qualifier (Con (UnQual (Ident "True"))))) []) => ListExp ((:) E [])

    rule ListComp E ((:) Q:QualStmt []) => ListComp E ((:) Q:QualStmt ((:) (QualStmt (Qualifier (Con (UnQual (Ident "True"))))) []))
        if Q:QualStmt =/=Bool QualStmt (Qualifier (Con (UnQual (Ident "True"))))

    rule ListComp E ((:) (QualStmt (Qualifier E1)) Qs:HsList) => If E1 (ListComp E Qs:HsList) (ListExp [])
        if Qs:HsList =/=Bool []

    rule <k>
             ListComp E ((:) (QualStmt (Generator _ P:Pat E1)) Qs:HsList) =>
                 Let (BDecls
                     ((:) (FunBind
                         ((:) (Match NoLoc (Ident FreshId) ((:) P:Pat []) Nothing
                           (UnGuardedRhs (ListComp E Qs:HsList)) (BDecls []))
                         ((:) (Match NoLoc (Ident FreshId) ((:) PWildCard []) Nothing
                           (UnGuardedRhs (ListExp [])) (BDecls []))
                         [])))
                     []))
                 (App (App (Var (UnQual (Ident "concatMap"))) (Var (UnQual (Ident FreshId)))) E1)
         <_/k>
         <fresh> N => N +Int 1 </fresh>
         if Qs:HsList =/=Bool []
         where FreshId = ("@ok" +String Int2String(N))


    rule ListComp E ((:) (QualStmt (LetStmt Bs:Binds)) Qs:HsList) => Let Bs:Binds (ListComp E Qs:HsList)
        if Qs:HsList =/=Bool []

    /*
    Do expressions satisfy these identities, which may be used as a
    translation into the kernel, after eliminating empty stmts:

    do {e}                = e
    do {e; stmts}         = e >> do {stmts}
    do {p <- e; stmts}    = let ok p = do {stmts}
                                ok _ = fail "..."
                            in e >>= ok
    do {let decls; stmts} = let decls in do {stmts}

    The ellipsis "..." stands for a compiler-generated error message, passed
    to fail, preferably giving some indication of the location of the
    pattern-match failure; the functions >>, >>=, and fail are operations in
    the class Monad, as defined in the Prelude; and ok is a fresh identifier.
    */

    rule Do ((:) (Qualifier E) []) => E

    rule Do ((:) (Qualifier E) Ss:HsList) => InfixApp E (QVarOp (UnQual (Symbol ">>"))) (Do Ss:HsList)
        if Ss:HsList =/=Bool []

    rule <k>
            Do ((:) (Generator _ P:Pat E) Ss) =>
                Let (BDecls
                    ((:) (FunBind
                        ((:) (Match NoLoc (Ident FreshId) ((:) P:Pat []) Nothing
                          (UnGuardedRhs (Do Ss)) (BDecls []))
                        ((:) (Match NoLoc (Ident FreshId) ((:) PWildCard []) Nothing
                          (UnGuardedRhs (App (Var (UnQual (Ident "fail")))
                            (Lit (StringLit "pattern match failure")))) (BDecls []))
                        [])))
                    []))
                (InfixApp E (QVarOp (UnQual (Symbol ">>="))) (Var (UnQual (Ident FreshId))))
         <_/k>
         <fresh> N => N +Int 1 </fresh>
         if Ss:HsList =/=Bool []
         where FreshId = ("@ok" +String Int2String(N))

    rule Do ((:) (LetStmt Bs) Ss:HsList) => Let Bs (Do Ss:HsList)
        if Ss:HsList =/=Bool []



    /*
    The general binding form for functions is semantically equivalent to the
    equation (i.e. simple pattern binding):

    x = \ x1 ... xk -> case (x1, ..., xk) of
        (p11, ..., p1k) match1
        ...
        (pn1, ..., pnk) matchn

    where the xi are new identifiers.
    */

    rule <k>
            FunBind Ms:HsList => PatBind NoLoc (PVar (Ident name)) Nothing
              (UnGuardedRhs
                (Lambda NoLoc (freshPVarList "x" N arity)
                  (Case (Tuple (freshVarList "x" N arity)) (trMatchesToAlts Ms:HsList))))
              (BDecls [])
         <_/k>
         <fresh> N => N +Int arity +Int 1 </fresh>
         where arity = getArity Ms:HsList
           and name  = getName  Ms:HsList

    /*
    The pattern binding

    p | g1 = e1
      | g2 = e2
      ...
      | gm = em
      where { decls }

    is semantically equivalent to this simple pattern binding:

    p = let decls in
            if g1 then e1 else
            if g2 then e2 else
            ...
            if gm then em else error "Unmatched pattern"

    */
    rule <k>
            PatBind Loc:SrcLoc Pat:Pat Type:HsMaybe Rhs:Rhs Binds:Binds =>
              desugared(PatBind Loc Pat Type
                (UnGuardedRhs (bindIfNonEmpty Binds (trRhsToExp Rhs)))
                (BDecls []))
         <_/k>


endkm
