(* ::Package:: *)

 


(* ::Section:: *)
(*CommutatorExplicit*)


(* ::Text:: *)
(*`CommutatorExplicit[exp]` substitutes any `Commutator` and `AntiCommutator` in `exp` by their definitions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Calc](Calc.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


DeclareNonCommutative[a,b,c,d]


Commutator[a,b]
CommutatorExplicit[%]


AntiCommutator[a-c,b-d]
CommutatorExplicit[%]


CommutatorExplicit[AntiCommutator[a-c,b-d]]//DotSimplify


UnDeclareNonCommutative[a,b,c,d]
