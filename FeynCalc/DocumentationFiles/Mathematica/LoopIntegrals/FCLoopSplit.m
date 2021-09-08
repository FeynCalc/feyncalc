(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopSplit*)


(* ::Text:: *)
(*`FCLoopSplit[exp, {q1, q2, ...}]` separates `exp` into the following four pieces: *)


(* ::Text:: *)
(*1) terms that are free of loop integrals*)


(* ::Text:: *)
(*2) terms with scalar loop integrals*)


(* ::Text:: *)
(*3) terms with tensor loop integrals, where all loop momenta are contracted*)


(* ::Text:: *)
(*4) terms with tensor loop integrals, where at least some loop momenta have free indices*)


(* ::Text:: *)
(*The result is returned as a list with the 4 above elements.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


FVD[q,\[Mu]] FAD[{q,m}]
FCLoopSplit[%,{q}]


x+GSD[p+q] FAD[{q,m}]
FCLoopSplit[%,{q}]
