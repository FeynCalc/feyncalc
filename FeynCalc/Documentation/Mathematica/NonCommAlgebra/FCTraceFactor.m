(* ::Package:: *)

 


(* ::Section:: *)
(*FCTraceFactor*)


(* ::Text:: *)
(*`FCTraceFactor[expr]` factors out all expressions inside a trace to which the trace doesn't apply. For example, all objects that are not Dirac matrices can be safely factored out from every Dirac trace.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracTrace](DiracTrace.md), [SUNTrace](SUNTrace.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Pull constants out of the Dirac trace*)


FCTraceFactor[DiracTrace[c1 . (c2*(GS[p1]+M)) . GA[\[Mu]] . (c3*(GS[p2]+M2))]]
