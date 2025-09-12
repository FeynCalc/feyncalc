(* ::Package:: *)

 


(* ::Section:: *)
(*CommutatorExplicit*)


(* ::Text:: *)
(*`CommutatorExplicit[exp]` substitutes any `FCCommutator` and `FCAntiCommutator` in `exp` by their definitions.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Calc](Calc.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


DeclareNonCommutative[a,b,c,d]


FCCommutator[a,b]

CommutatorExplicit[%]


FCAntiCommutator[a-c,b-d]

CommutatorExplicit[%]


CommutatorExplicit[FCAntiCommutator[a-c,b-d]]//DotSimplify


UnDeclareNonCommutative[a,b,c,d]
