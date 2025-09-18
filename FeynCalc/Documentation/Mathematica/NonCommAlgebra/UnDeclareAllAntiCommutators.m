(* ::Package:: *)

 


(* ::Section:: *)
(*UnDeclareAllAntiCommutators*)


(* ::Text:: *)
(*`UnDeclareAllAntiCommutators[]` undeclares all user-defined anti-commutators.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCAntiCommutator](FCAntiCommutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


DeclareNonCommutative[a,b,c,d]

FCAntiCommutator[a,b]=x1;

FCAntiCommutator[c,d]=x2;

DotSimplify[a . b . c . d]


UnDeclareAllAntiCommutators[]

DotSimplify[a . b . c . d]
