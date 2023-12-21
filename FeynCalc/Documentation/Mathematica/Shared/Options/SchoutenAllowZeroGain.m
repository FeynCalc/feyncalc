(* ::Package:: *)

 


(* ::Section:: *)
(*SchoutenAllowZeroGain*)


(* ::Text:: *)
(*`SchoutenAllowZeroGain` is an option for `FCSchoutenBruteForce` and other functions that attempt to simplify the input expression by applying Schouten's identity. When set to `True`, the algorithm would apply Schouten's identity to the given expression even if this does not decrease the total number of terms in the expression. This is sometimes useful when the algorithm gets stuck and cannot find further transformation that would make the expression shorter.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCSchoutenBruteForce](FCSchoutenBruteForce.md).*)


(* ::Subsection:: *)
(*Examples*)
