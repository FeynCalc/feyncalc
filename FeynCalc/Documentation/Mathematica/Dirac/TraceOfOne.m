(* ::Package:: *)

 


(* ::Section:: *)
(*TraceOfOne*)


(* ::Text:: *)
(*`TraceOfOne` is an option for `Tr` and `DiracTrace`. Its setting determines the value of the unit trace.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracSimplify](DiracSimplify.md), [DiracTrace](DiracTrace.md).*)


(* ::Subsection:: *)
(*Examples*)


DiracTrace[1]
DiracSimplify[%]


DiracTrace[1,TraceOfOne->tr1]
DiracSimplify[%]
