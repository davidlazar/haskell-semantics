kmod HASKELL-DESUGARING is
    including HASKELL-SYNTAX
    including K

    kvar E E1 E2 E3 : Exp

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
