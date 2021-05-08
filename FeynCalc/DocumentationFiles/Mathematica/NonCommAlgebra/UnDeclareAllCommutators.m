 
(* ::Section:: *)
(* UnDeclareAllCommutators *)
(* ::Text:: *)
(*UnDeclareAllCommutators[] undeclares all user-defined commutators..*)


(* ::Subsection:: *)
(* Examples *)
DeclareNonCommutative[a,b,c,d]
Commutator[a,b]=x1;
Commutator[c,d]=x2;
DotSimplify[a.b.c.d]

UnDeclareAllCommutators[]
DotSimplify[a.b.c.d]
