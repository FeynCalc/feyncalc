(* ::Package:: *)

 


(* ::Section:: *)
(*UnDeclareAllAntiCommutators*)


(* ::Text:: *)
(*`UnDeclareAllAntiCommutators[]` undeclares all user-defined anticommutators.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [AntiCommutator](AntiCommutator.md), [CommutatorExplicit](CommutatorExplicit.md), [DeclareNonCommutative](DeclareNonCommutative.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


DeclareNonCommutative[a,b,c,d]
AntiCommutator[a,b]=x1;
AntiCommutator[c,d]=x2;
DotSimplify[a . b . c . d]


UnDeclareAllAntiCommutators[]
DotSimplify[a . b . c . d]
