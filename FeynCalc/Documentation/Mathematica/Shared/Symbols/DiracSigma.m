(* ::Package:: *)

 


(* ::Section:: *)
(*DiracSigma*)


(* ::Text:: *)
(*`DiracSigma[a, b]` stands for $I/2(a.b-b.a)$ in 4 dimensions.*)


(* ::Text:: *)
(*`a` and `b` must have head `DiracGamma`, `GA` or `GS`. Only antisymmetry is implemented.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracSigmaExplicit](DiracSigmaExplicit.md).*)


(* ::Subsection:: *)
(*Examples*)


DiracSigma[GA[\[Alpha]],GA[\[Beta]]]
DiracSigmaExplicit[%]


DiracSigma[GA[\[Beta]],GA[\[Alpha]]]


DiracSigma[GS[p],GS[q]]
DiracSigmaExplicit[%]


(* ::Text:: *)
(*The antisymmetry property is built-in*)


DiracSigma[GA[\[Alpha]],GA[\[Alpha]]]
