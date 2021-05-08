 
(* ::Section:: *)
(* AntiCommutator *)
(* ::Text:: *)
(*AntiCommutator[x, y] = c defines the anti-commutator of the non commuting objects $\text{x}$ and $\text{y}$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Commutator, CommutatorExplicit, DeclareNonCommutative, DotSimplify.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*This declares a and b as noncommutative variables.*)


DeclareNonCommutative[a,b]
AntiCommutator[a,b]

CommutatorExplicit[%]

CommutatorExplicit[AntiCommutator[a+b,a-2b ]]

DotSimplify[AntiCommutator[a+b,a-2b ]]

DeclareNonCommutative[c,d,Overscript[c, ~],Overscript[d, ~]]

(* ::Text:: *)
(*Defining {c,d} = z results in replacements of c.d by z-d.c.*)


AntiCommutator[c,d] = z

DotSimplify[ d . c . d ]

AntiCommutator[Overscript[d, ~],Overscript[c, ~]] = Overscript[z, ~]

DotSimplify[ Overscript[d, ~] . Overscript[c, ~] . Overscript[d, ~] ]

UnDeclareNonCommutative[a,b,c,d,Overscript[c, ~],Overscript[d, ~]]
Unset[AntiCommutator[c,d]]
Unset[AntiCommutator[Overscript[d, ~],Overscript[c, ~]]]