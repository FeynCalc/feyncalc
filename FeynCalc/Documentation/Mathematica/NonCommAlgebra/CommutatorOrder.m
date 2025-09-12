(* ::Package:: *)

 


(* ::Section:: *)
(*CommutatorOrder*)


(* ::Text:: *)
(*`CommutatorOrder[exp]` orders any `FCCommutator` and `FCAntiCommutator` lexicographically.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCCommutator](FCCommutator.md), [FCAntiCommutator](FCAntiCommutator.md).*)


(* ::Subsection:: *)
(*Examples*)


FCCommutator[a, b] + FCCommutator[b, a]

CommutatorOrder[%]
