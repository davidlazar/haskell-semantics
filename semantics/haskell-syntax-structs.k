/*
    Syntactic structures existent in Haskell's abstract syntax that need to be
    marshalled into Maude.
*/

kmod HASKELL-SYNTAX-STRUCTS is
    including K

    syntax HsList ::= []
                    | (:) K HsList

    syntax HsMaybe ::= `Just K
                     | `Nothing

    syntax HsTuple ::= (,) K K

    syntax HsRational ::= Int % Int

    syntax K ::= Int
               | Bool
               | String
               | HsList
               | HsMaybe
               | HsTuple
               | HsRational

endkm
