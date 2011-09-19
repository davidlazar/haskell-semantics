kmod HASKELL-DESUGARING is
    including HASKELL-SYNTAX
    including HASKELL-DESUGARING-HELPERS
    including K
    including VISITOR


    /*
    Eventually sorts will be inferred automatically. For now, declare sorts
    explicitly in one big kvar block.
    */
    kvar E E1 E2 E3 : Exp
    kvar I I1 I2 N : #Int

    kvar K         : K
    kvar Kl        : List{K}
    kvar KResult   : KResult
    kvar Exp       : Exp
    kvar Exp'      : Exp
    kvar Exp0      : Exp
    kvar Exp1      : Exp
    kvar BExp      : Exp
    kvar Rhs       : Rhs
    kvar Binds     : Binds
    kvar Alts      : HsList
    kvar Decls     : HsList
    kvar Exps      : HsList
    kvar GAlts     : HsList
    kvar GRhss     : HsList
    kvar Matches   : HsList
    kvar Pats      : HsList
    kvar QualStmts : HsList
    kvar Stmts     : HsList
    kvar Pat       : Pat
    kvar Alt       : Alt
    kvar Fresh     : #String
    kvar Literal   : Literal
    kvar Name      : Name
    kvar QName     : QName
    kvar QOp       : QOp
    kvar MaybeType : HsMaybe
    kvar Pat1      : Pat
    kvar Pat2      : Pat


    configuration
        <T multiplicity="?">
            <k> .K </k>
            <fresh> 0 </fresh>
        </T>
        <result multiplicity="?"> .K </result>


    /*
    Helper to initialize the K cell with a program.
    */
    syntax Bag ::= [[ K ]]
    macro [[ K ]] = <T>... <k> K </k> ...</T>


    /*
    When the K cell is left with a KResult, move the result program to the
    result cell and perform post-desugaring tasks.
    */

    rule <T>... <k> KResult </k> ...</T> =>
           <result> desimplifyProgram(desugaredProgram(eraseKLabel(desugared, KResult))) </result>

    syntax K ::= desugaredProgram List{K}
    rule desugaredProgram(K) => K


    /*
    Use the simplified K visitor pattern to remove intermediate syntax.
    */

    syntax K ::= desimplifyProgram K
    macro desimplifyProgram K = visit K applying desimplify if label in SetItem(wklabel('SimplePatBind____)) SetItem(wklabel('SimpleCase____))

    syntax KLabel ::= desimplify

    rule desimplify(SimplePatBind Loc Pat MaybeType Exp) => PatBind Loc Pat MaybeType (UnGuardedRhs Exp) (BDecls [])

    rule desimplify(SimpleCase Exp0 Pat Exp Exp') =>
           Case Exp0 ((:) (Alt NoLoc Pat (UnGuardedAlt Exp) (BDecls []))
                     ((:) (Alt NoLoc PWildCard (UnGuardedAlt Exp') (BDecls [])) []))

    /*
    Wrapper to explicitly mark a piece of syntax as desugared (used
    sparingly).
    */

    syntax KResultLabel ::= desugared


    /*
    The integer literal i is equivalent to fromInteger i, where fromInteger is
    a method in class Num (see Section 6.4.1).
    */

    rule <k> Lit (IntLit I) => desugared(App (Var (UnQual (Ident "fromInteger"))) (Lit (IntLit I))) ...</k>

    /*
    The floating point literal f is equivalent to fromRational (n Ratio.% d),
    where fromRational is a method in class Fractional and Ratio.% constructs
    a rational from two integers, as defined in the Ratio library. The
    integers n and d are chosen so that n/d = f.
    */

    rule Lit (Frac (:% I1 I2)) => App (Var (UnQual (Ident "fromRational")))
      (InfixApp (Lit (IntLit I1)) (QVarOp (Qual (ModuleName "Ratio") (Symbol "%"))) (Lit (IntLit I2)))

    /*
    The following identity holds:

    \ p1 ... pn -> e = \ x1 ... xn -> case (x1, ..., xn) of (p1, ..., pn) -> e

    where the xi are new identifiers.
    */

    rule <k>
            Lambda Loc Pats Exp => Lambda Loc freshPVars
              (Case (listToTuple (freshVarList "x" N arity))
              ((:) (Alt NoLoc (listToPTuple Pats) (UnGuardedAlt Exp) (BDecls [])) []))
         ...</k>
         <fresh> N => N +Int arity +Int 1 </fresh>
         if anyNonTrivialPats Pats
         where arity = (length Pats)
           and freshPVars = (freshPVarList "x" N arity)

    rule <k> Lambda Loc Pats Exp => desugared(Lambda Loc Pats Exp) ...</k>
         if notBool (anyNonTrivialPats Pats)


    syntax #Bool ::= anyNonTrivialPats HsList
    rule anyNonTrivialPats [] => false
        [structural]
    rule anyNonTrivialPats ((:) Pat Pats) => true
        if getKLabel Pat =/=Bool 'PVar_
        [structural]
    rule anyNonTrivialPats ((:) (PVar _) Pats) => anyNonTrivialPats Pats
        [structural]


    /*
    The following identities hold:

    e1 op e2 = (op) e1 e2
    -e       = negate (e)

    */

    rule NegApp E => App (Var (UnQual (Ident "negate"))) E

    rule InfixApp E1 QOp E2 => App (App fromInfix(QOp) E1) E2

    syntax Exp ::= fromInfix QOp
    rule fromInfix(QVarOp QName) => Var QName [structural]
    rule fromInfix(QConOp QName) => Con QName [structural]

    rule PInfixApp Pat1 QName Pat2 => PApp QName ((:) Pat1 ((:) Pat2 []))


    /*
    The following identities hold:

    (op e) = \ x -> x op e
    (e op) = \ x -> e op x

    where op is a binary operator, e is an expression, and x is a variable that
    does not occur free in e.
    */

    rule <k>
             RightSection QOp E =>
               Lambda NoLoc ((:) (PVar (Ident FreshId)) [])
                 (InfixApp (Var (UnQual (Ident FreshId))) QOp E)
         ...</k>
         <fresh> N => N +Int 1 </fresh>
         where FreshId = ("@x" +String Int2String(N))

    rule <k>
             LeftSection E QOp =>
               Lambda NoLoc ((:) (PVar (Ident FreshId)) [])
                 (InfixApp E QOp (Var (UnQual (Ident FreshId))))
         ...</k>
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
    The following identity holds:

    [e1, ..., ek] = e1 : (e2 : ( ... (ek : [])))

    where : and [] are constructors for lists, as defined in the Prelude (see
    Section 6.1.3). The types of e1 through ek must all be the same (call it
    t), and the type of the overall expression is [t] (see Section 4.1.2).
    */

    rule <k> ListExp [] => desugared(ListExp []) ...</k>

    rule ListExp ((:) Exp Exps) => InfixApp Exp (QConOp (Special Cons)) (ListExp Exps)


    rule PList ((:) Pat []) => PApp (Special Cons) ((:) Pat ((:) (PList []) []))

    rule PList ((:) Pat Pats) => PApp (Special Cons) ((:) Pat ((:) (PList Pats) []))
        if nonempty Pats


    /*
    (e1, ..., ek) for k >= 2 is an instance of a k-tuple as defined in the
    Prelude, and requires no translation. If t1 through tk are the types
    of e1 through ek, respectively, then the type of the resulting tuple
    is (t1, ..., tk) (see Section 4.1.2).
    */

    /*
    NB: Although the Report does not specify any translation for tuples,
    we apply the following equation

        (e_1, ..., e_n) => (,,, ... (n-1 commas)) e_1 ... e_n

    in order to simplify the pattern matching semantics; tuples would be a
    special case otherwise.
    */

    rule Tuple Exps => uninfixTuple (Con (Special (TupleCon Boxed (length Exps)))) Exps

    rule PTuple Pats => PApp (Special (TupleCon Boxed (length Pats))) Pats

    syntax Exp ::= uninfixTuple Exp HsList
    rule uninfixTuple Exp ((:) Exp' []) => App Exp Exp'
        [structural]
    rule uninfixTuple Exp ((:) Exp' Exps) => uninfixTuple (App Exp Exp') Exps
        if nonempty Exps
        [structural]


    /*
    (e) is equivalent to e.
    */

    rule Paren E => E

    rule PParen Pat => Pat


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

    rule ListComp E ((:) (QualStmt (Qualifier E1)) QualStmts) => If E1 (ListComp E QualStmts) (ListExp [])
        if nonempty QualStmts

    rule <k>
             ListComp E ((:) (QualStmt (Generator _ Pat E1)) QualStmts) =>
                 Let (BDecls
                     ((:) (FunBind
                         ((:) (Match NoLoc (Ident FreshId) ((:) Pat []) Nothing
                           (UnGuardedRhs (ListComp E QualStmts)) (BDecls []))
                         ((:) (Match NoLoc (Ident FreshId) ((:) PWildCard []) Nothing
                           (UnGuardedRhs (ListExp [])) (BDecls []))
                         [])))
                     []))
                 (App (App (Var (UnQual (Ident "concatMap"))) (Var (UnQual (Ident FreshId)))) E1)
         ...</k>
         <fresh> N => N +Int 1 </fresh>
         if nonempty QualStmts
         where FreshId = ("@ok" +String Int2String(N))


    rule ListComp E ((:) (QualStmt (LetStmt Binds)) QualStmts) => Let Binds (ListComp E QualStmts)
        if nonempty QualStmts

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

    rule Do ((:) (Qualifier E) Stmts) => InfixApp E (QVarOp (UnQual (Symbol ">>"))) (Do Stmts)
        if nonempty Stmts

    rule <k>
            Do ((:) (Generator _ Pat E) Stmts) =>
                Let (BDecls
                    ((:) (FunBind
                        ((:) (Match NoLoc (Ident FreshId) ((:) Pat []) Nothing
                          (UnGuardedRhs (Do Stmts)) (BDecls []))
                        ((:) (Match NoLoc (Ident FreshId) ((:) PWildCard []) Nothing
                          (UnGuardedRhs (App (Var (UnQual (Ident "fail")))
                            (Lit (StringLit "pattern match failure")))) (BDecls []))
                        [])))
                    []))
                (InfixApp E (QVarOp (UnQual (Symbol ">>="))) (Var (UnQual (Ident FreshId))))
         ...</k>
         <fresh> N => N +Int 1 </fresh>
         if nonempty Stmts
         where FreshId = ("@ok" +String Int2String(N))

    rule Do ((:) (LetStmt Binds) Stmts) => Let Binds (Do Stmts)
        if nonempty Stmts



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
            FunBind Matches => PatBind NoLoc (PVar (Ident name)) Nothing
              (UnGuardedRhs
                (Lambda NoLoc (freshPVarList "x" N arity)
                  (Case (listToTuple (freshVarList "x" N arity)) (trMatchesToAlts Matches))))
              (BDecls [])
         ...</k>
         <fresh> N => N +Int arity +Int 1 </fresh>
         where arity = getFunctionArity Matches
           and name  = getFunctionName  Matches


    syntax HsList ::= trMatchesToAlts HsList
    rule trMatchesToAlts [] => []
        [structural]
    rule trMatchesToAlts ((:) (Match _ _ Pats _ Rhs Binds) Matches) =>
        (:) (Alt NoLoc (listToPTuple Pats) (trRhsToGuardedAlts Rhs) Binds)
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

    syntax Decl ::= `SimplePatBind SrcLoc Pat HsMaybe Exp [strict hybrid]

    rule <k>
            PatBind Loc:SrcLoc Pat MaybeType Rhs Binds =>
              SimplePatBind Loc Pat MaybeType
                (bindIfNonEmpty Binds (trRhsToExp Rhs))
         ...</k>


    syntax Exp ::= trRhsToExp Rhs
    rule trRhsToExp (UnGuardedRhs Exp) => Exp
        [structural]
    rule trRhsToExp (GuardedRhss GRhss) => trGRhssToExp GRhss
        [structural]


    syntax Exp ::= trGRhssToExp HsList
    rule trGRhssToExp [] => App (Var (UnQual (Ident "error"))) (Lit (StringLit "Unmatched pattern"))
        [structural]
    rule trGRhssToExp ((:) (GuardedRhs _ ((:) (Qualifier BExp) []) Exp) GRhss) =>
        If BExp Exp (trGRhssToExp GRhss)
        [structural]


    /*
    The dynamic semantics of the expression let { d1 ; ... ; dn } in e0 are
    captured by this translation: After removing all type signatures, each
    declaration di is translated into an equation of the form pi = ei, where
    pi and ei are patterns and expressions respectively, using the translation
    in Section 4.4.3. Once done, these identities hold, which may be used as a
    translation into the kernel:
    */

    /*
    let {p1=e1; ...; pn=en} in e0  =  let (~p1, ..., ~pn) = (e1, ..., en) in e0
    */

    rule <k>
            Let (BDecls Decls) Exp =>
              Let (BDecls ((:) (PatBind NoLoc (PTuple (getIrrPats Decls))
                Nothing (UnGuardedRhs (Tuple (getExps Decls)))
                (BDecls [])) [])) Exp
         ...</k>
         if length Decls >Int 1


    syntax HsList ::= getIrrPats HsList
    rule getIrrPats [] => []
        [structural]
    rule getIrrPats ((:) (SimplePatBind _ Pat _ _) Decls) => (:) (PIrrPat Pat) (getIrrPats Decls)
        [structural]


    syntax HsList ::= getExps HsList
    rule getExps [] => []
        [structural]
    rule getExps ((:) (SimplePatBind _ _ _ Exp) Decls) => (:) Exp (getExps Decls)
        [structural]

    /*
    let p = e1 in e0  =  case e1 of ~p -> e0
        where no variable in p appears free in e1
    */
    // TODO: For now, we skip this equation and blindly apply the equation
    // below.

    /*
    let p = e1 in e0  =  let p = fix ( \ ~p -> e1) in e0
    */

    rule <k>
            Let (BDecls ((:) (SimplePatBind _ Pat MaybeType Exp1) [])) Exp0 =>
              Case (App (Var (UnQual (Ident "@fix"))) (Lambda NoLoc ((:) (PIrrPat Pat) []) Exp1))
                ((:) (Alt NoLoc (PIrrPat Pat) (UnGuardedAlt Exp0) (BDecls [])) [])
         ...</k>


    /*
    Formal Semantics of Pattern Matching:
    */

    // TODO: equation (a)
    // TODO: the sort of V and the sorts of intermediate syntax will change
    // once equation (a) is implemented.
    kvar V : Exp

    /*
    (b)

       case v of { p1 match1;  ... ; pn matchn }
    =  case v of { p1 match1 ;
                    _ -> ... case v of {
                               pn matchn ;
                                _ -> error "No match" }...}

      where each matchi has the form:

      | gi,1 -> ei,1 ; ... ; | gi,mi -> ei,mi where { declsi }

    */

    syntax Exp ::= `NestedCase Exp Alt Exp [strict]

    rule <k>
            Case V Alts => trAltsToNestedCase V Alts
         ...</k>
         if notBool (singlePVar Alts)


    syntax Exp ::= trAltsToNestedCase Exp HsList
    rule trAltsToNestedCase _ [] => App (Var (UnQual (Ident "error"))) (Lit (StringLit "No match"))
        [structural]
    rule trAltsToNestedCase V ((:) Alt Alts) => NestedCase V Alt (trAltsToNestedCase V Alts)
        [structural]


    // TODO: we need a better name for this side condition.
    syntax #Bool ::= singlePVar HsList
    rule singlePVar ((:) (Alt _ (PVar _) (UnGuardedAlt _) (BDecls [])) []) => true
        [structural]
    rule singlePVar ((:) (Alt _ Pat GAs:GuardedAlts Binds) Alts) => false
        if nonempty Alts orBool getKLabel Pat =/=Bool 'PVar_ orBool getKLabel GAs =/=Bool 'UnGuardedAlt_ orBool Binds =/=Bool (BDecls [])
        [structural]


    /*
    (c)

      case v of { p | g1 -> e1 ; ...
                    | gn -> en where { decls }
                  _      -> e' }
    = case e' of
      {y ->  (where y is a new variable)
       case v of {
             p -> let { decls } in
                    if g1 then e1 ... else if gn then en else y ;
             _ -> y }}

    */

    syntax Exp ::= `SimpleCase Exp Pat Exp Exp [strict]

    rule <k>
            NestedCase V (Alt _ Pat (UnGuardedAlt Exp1) Binds) Exp' =>
              SimpleCase V Pat (bindIfNonEmpty Binds Exp1) Exp'
         ...</k>

    rule <k>
            NestedCase V (Alt _ Pat (GuardedAlts GAlts) Binds) Exp' =>
             Case Exp' ((:) (Alt NoLoc (PVar (Ident FreshId)) (UnGuardedAlt
               (SimpleCase V Pat (bindIfNonEmpty Binds (trGAltsToExp FreshId GAlts))
                 (Var (UnQual (Ident FreshId))))) (BDecls [])) [])
         ...</k>
         <fresh> N => N +Int 1 </fresh>
         where FreshId = ("@y" +String Int2String(N))


    syntax Exp ::= trGAltsToExp #String HsList
    rule trGAltsToExp Fresh [] => Var (UnQual (Ident Fresh))
    rule trGAltsToExp Fresh ((:) (GuardedAlt _ ((:) (Qualifier BExp) []) Exp) GAlts) =>
           If BExp Exp (trGAltsToExp Fresh GAlts)


    /*
    (d)

      case v of { ~p -> e; _ -> e' }
    = (\x1 ... xn -> e) (case v of { p -> x1 }) ... (case v of { p -> xn })
      where x1, ..., xn are all the variables in p

    */

    rule <k> SimpleCase V (PIrrPat Pat) Exp Exp' =>
             unrollIrrPat V Pat PVars (Lambda NoLoc (kListToHsList PVars) Exp)
         ...</k>
         where PVars = (extractK 'PVar_ Pat)

    syntax Exp ::= unrollIrrPat Exp Pat List{K} Exp
    rule unrollIrrPat _ _ .List{K} Exp => Exp
        [structural]
    rule unrollIrrPat V Pat ((PVar Name) ,, Kl) Exp => unrollIrrPat V Pat Kl
      (App Exp (Case V ((:) (Alt NoLoc Pat (UnGuardedAlt (Var (UnQual Name))) (BDecls [])) [])))
        [structural]


    /*
    (e)

    case v of { x@p -> e; _ -> e' } = case v of { p -> ( \ x -> e ) v ; _ -> e' }

    */

    rule <k> SimpleCase V (PAsPat Name Pat) Exp Exp' =>
             SimpleCase V Pat (Lambda NoLoc ((:) (PVar Name) []) Exp) Exp'
         ...</k>


    /*
    (f)

    case v of { _ -> e; _ -> e' } = e

    */

    rule <k> SimpleCase V PWildCard Exp _ => Exp ...</k>


    /*
    (h)

    case v of { k -> e; _ -> e' } = if (v==k) then e else e'
    where k is a numeric, character, or string literal. 

    */

    rule <k>
            SimpleCase V (PLit Literal) Exp Exp' =>
              If (InfixApp V (QVarOp (UnQual (Symbol "=="))) (Lit Literal)) Exp Exp'
         ...</k>


    /*
    (i)

    case v of { x -> e; _ -> e' } = case v of { x -> e }

    */

    rule <k>
            SimpleCase V (PVar Name) Exp _ =>
              Case V ((:) (Alt NoLoc (PVar Name) (UnGuardedAlt Exp) (BDecls [])) [])
         ...</k>


    /*
    (j)

    case v of { x -> e } = ( \ x -> e ) v

    */

    rule <k>
            Case V ((:) (Alt _ (PVar Name) (UnGuardedAlt Exp) (BDecls [])) []) =>
              App (Lambda NoLoc ((:) (PVar Name) []) Exp) V
         ...</k>


    /*
    Catch-all for simple case expressions unaccounted for above.
    */

    rule <k> SimpleCase V Pat Exp Exp' => desugared(SimpleCase V Pat Exp Exp') ...</k>
        if Pat =/=Bool PWildCard andBool getKLabel Pat =/=Bool 'PVar andBool getKLabel Pat =/=Bool 'PLit andBool getKLabel Pat =/=Bool 'PIrrPat_


endkm
