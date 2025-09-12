(* ::Package:: *)

 


(* ::Section:: *)
(*FCCommutator*)


(* ::Text:: *)
(*`FCCommutator[x, y] = c` defines the commutator between the (non-commuting) objects `x` and `y`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCAntiCommutator](FCAntiCommutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


DeclareNonCommutative[a,b,c,d]


FCCommutator[a,b]

CommutatorExplicit[%]


DotSimplify[FCCommutator[a+b,c+d]] 

UnDeclareNonCommutative[a,b,c,d]


(* ::Text:: *)
(*Verify the Jacobi identity.*)


\[Chi]=FCCommutator; DeclareNonCommutative[x,y,z];


\[Chi][x,\[Chi][y,z]]+\[Chi][y,\[Chi][z,x]]+\[Chi][z,\[Chi][x,y]]

DotSimplify[%]


Clear[\[Chi]]

UnDeclareNonCommutative[x,y,z]
