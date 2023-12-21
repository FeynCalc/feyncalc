(* ::Package:: *)

 


(* ::Section:: *)
(*ClearHeads*)


(* ::Text:: *)
(*`ClearHeads` is an option of `FCLoopIsolate`, `FCDiracIsolate` and other functions. It takes a list of heads that will be replaced by `Identity` in the isolating function. This is useful for cases when we first apply the isolating function to an expression, then simplify the isolated expression and finally want to apply the isolating function again to pull out the simplified expressions out of the old heads.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopIsolate](FCLoopIsolate.md), [FCDiracIsolate](FCDiracIsolate.md).*)


(* ::Subsection:: *)
(*Examples*)
