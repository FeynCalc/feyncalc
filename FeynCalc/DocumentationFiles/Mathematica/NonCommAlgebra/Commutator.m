(* ::Package:: *)

 


(* ::Section:: *)
(*Commutator*)


(* ::Text:: *)
(*`Commutator[x, y] = c` defines the commutator between the (non-commuting) objects `x` and `y`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [AntiCommutator](AntiCommutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


DeclareNonCommutative[a,b,c,d]


Commutator[a,b]
CommutatorExplicit[%]


DotSimplify[Commutator[a+b,c+d]] 
UnDeclareNonCommutative[a,b,c,d]


(* ::Text:: *)
(*Verify the Jacobi identity.*)


\[Chi]=Commutator; DeclareNonCommutative[x,y,z];


\[Chi][x,\[Chi][y,z]]+\[Chi][y,\[Chi][z,x]]+\[Chi][z,\[Chi][x,y]]
DotSimplify[%]


Clear[\[Chi]]
UnDeclareNonCommutative[x,y,z]
