(* ::Package:: *)

 


(* ::Section:: *)
(*UnDeclareAllCommutators*)


(* ::Text:: *)
(*`UnDeclareAllCommutators[]` undeclares all user-defined commutators.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Commutator](Commutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


DeclareNonCommutative[a,b,c,d]
Commutator[a,b]=x1;
Commutator[c,d]=x2;
DotSimplify[a . b . c . d]


UnDeclareAllCommutators[]
DotSimplify[a . b . c . d]
