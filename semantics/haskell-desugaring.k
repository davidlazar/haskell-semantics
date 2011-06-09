kmod HASKELL-DESUGARING is
    including HASKELL-SYNTAX
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
    kvar N : Int

    syntax SrcLoc ::= `NoLoc
    macro NoLoc = SrcLoc "<unknown.hs>" 0 0

    /*
    The following identities hold:
    e1 op e2 = (op) e1 e2
    -e       = negate (e)
    */

    macro NegApp E = App (Var (UnQual (Ident "negate"))) E

    macro InfixApp E1 Q:QOp E2 = App (App fromInfix(Q:QOp) E1) E2

    syntax Exp ::= fromInfix QOp
    macro fromInfix(QVarOp QN:QName) = Var QN:QName
    macro fromInfix(QConOp QN:QName) = Con QN:QName

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

    macro If E E1 E2 = Case E ((:) Alt NoLoc (PApp (UnQual (Ident "True" )) []) (UnGuardedAlt E1) (BDecls [])
                               (:) Alt NoLoc (PApp (UnQual (Ident "False")) []) (UnGuardedAlt E2) (BDecls [])
                               [])


    /*
    (e) is equivalent to e.
    */

    macro Paren E = E


    /*
    Arithmetic sequences satisfy these identities:
    [e1 ..]        = enumFrom e1
    [e1, e2 ..]    = enumFromThen e1 e2
    [e1 .. e3]     = enumFromTo e1 e3
    [e1, e2 .. e3] = enumFromThenTo e1 e2 e3
    where enumFrom, enumFromThen, enumFromTo, and enumFromThenTo are class
    methods in the class Enum as defined in the Prelude (see Figure 6.1).
    */

    macro EnumFrom E1 = App (Var (UnQual (Ident "enumFrom"))) E1
    macro EnumFromThen E1 E2 = App (App (Var (UnQual (Ident "enumFromThen"))) E1) E2
    macro EnumFromTo E1 E3 = App (App (Var (UnQual (Ident "enumFromTo"))) E1) E3
    macro EnumFromThenTo E1 E2 E3 = App (App (App (Var (UnQual (Ident "enumFromThenTo"))) E1) E2) E3

endkm
