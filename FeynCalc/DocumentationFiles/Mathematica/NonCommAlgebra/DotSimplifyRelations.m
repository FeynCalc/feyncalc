(* ::Package:: *)

 


(* ::Section:: *)
(*DotSimplifyRelations*)


(* ::Text:: *)
(*`DotSimplifyRelations` is an option for `DotSimplify`. Its setting may be a list of substitution rules of the form `DotSimplifyRelations -> {a.b -> c, b^2 -> 0, ...}`.*)


(* ::Text:: *)
(*In the rules, `Condition` should not be used and patterns should be avoided on the right-hand sides.*)


(* ::Text:: *)
(*Notice that the performance of `DotSimplify` scales very badly with the complexity of `DotSimplifyRelations` and the number of terms of the expression.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DotSimplify](DotSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)
