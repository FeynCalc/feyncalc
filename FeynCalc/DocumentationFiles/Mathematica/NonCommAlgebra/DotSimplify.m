 
(* ::Section:: *)
(* DotSimplify *)
(* ::Text:: *)
(*DotSimplify[exp] expands and reorders noncommutative terms in exp. Simplifying relations may be specified by the option DotSimplifyRelations or by Commutator and AntiCommutator definitions. Whether exp is expanded noncommutatively depends on the option Expanding..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*AntiCommutator, Commutator, Calc.*)



(* ::Subsection:: *)
(* Examples *)



GA[\[Mu]].(2 GS[p]-GS[q]).GA[\[Nu]]

DotSimplify[%]

DeclareNonCommutative[a,b,c]
a.(b-z c).a

DotSimplify[%]

Commutator[a,c]=1

DotSimplify[a.(b-z c).a]

Commutator[a,c]=.
DotSimplify[a.(b-z c).a]

AntiCommutator[b,a]=c

DotSimplify[a.(b-z c).a]

AntiCommutator[b,a]=.
DotSimplify[a.(b-z c).a,DotSimplifyRelations->{a.c->1/z}]

UnDeclareNonCommutative[a,b,c]
DeclareNonCommutative[x]
DotSimplify[x.x.x]

DotSimplify[x.x.x,DotPower->True]

UnDeclareNonCommutative[x]