/*
    Syntactic structures existent in Haskell's abstract syntax that need to be
    marshalled into Maude.
*/

kmod HASKELL-SYNTAX-STRUCTS is
    including K

    syntax HsList ::= []           [hybrid]
                    | (:) K HsList [strict hybrid]

    syntax HsMaybe ::= `Nothing    [hybrid]
                     | `Just K     [strict hybrid]

    syntax HsTuple ::= (,) K K

    syntax HsRational ::= :% Int Int  [hybrid]

    syntax K ::= Int
               | Bool
               | String
               | HsList
               | HsMaybe
               | HsTuple
               | HsRational

    syntax KResult ::= Int
                     | Bool
                     | String

endkm
