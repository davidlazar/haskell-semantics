/*
This is a simplified variant of the K visitor pattern included in the K
framework.
*/
kmod VISITOR is including K + K-WRAPPERS
    syntax K ::= visit List{K} applying KLabel if label in Set 
      [latex "\mbox{apply }{#2}\mbox{ to }{#3}\mbox{ in }{#1}"]

    kvar K1     : K
    kvar K2     : K
    kvar A      : KLabel
    kvar Label  : KLabel
    kvar Kl     : List{K}
    kvar NeKl   : NeList{K}
    kvar Labels : Set

    rule visit Label(Kl) applying A if label in Labels SetItem(wklabel(Label))
      => A(Label(visit Kl applying A if label in Labels SetItem(wklabel(Label))))
      [structural]

    rule visit Label(Kl) applying A if label in Labels
      => Label(visit Kl applying A if label in Labels)
      if notBool(wklabel(Label) in Labels)  
      [structural]

    rule visit . applying A if label in Labels => .
      [structural]

    rule visit K1 ~> K2 applying A if label in Labels 
      => visit K1 applying A if label in Labels
         ~>
         visit K2 applying A if label in Labels
      if K1 =/=Bool . andBool  K2 =/=Bool . 
      [structural]

    rule visit .List{K} applying A if label in Labels 
      => .List{K}
      [structural]

    rule visit K1,,NeKl applying A if label in Labels
      => visit K1 applying A if label in Labels
         ,,
         visit NeKl applying A if label in Labels
      [structural]

endkm
