 
(* ::Section:: *)
(* CommutatorExplicit *)
(* ::Text:: *)
(*CommutatorExplicit[exp] substitutes any Commutator and AntiCommutator in exp by their definitions..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Calc, DotSimplify.*)



(* ::Subsection:: *)
(* Examples *)



DeclareNonCommutative[a,b,c,d]
Commutator[a,b]

CommutatorExplicit[%]

AntiCommutator[a-c,b-d]

CommutatorExplicit[%]

CommutatorExplicit[%%]//DotSimplify

UnDeclareNonCommutative[a,b,c,d]