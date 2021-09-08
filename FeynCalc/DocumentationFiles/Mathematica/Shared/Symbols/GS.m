(* ::Package:: *)

 


(* ::Section:: *)
(*GS*)


(* ::Text:: *)
(*`GS[p]` can be used as input for a 4-dimensional $p^\mu \gamma_\mu$ and is transformed into `DiracGamma[Momentum[p]]` by `FeynCalcInternal` (=`FCI`).*)


(* ::Text:: *)
(*`GS[p,q, ...]` is a short form for `GS[p].GS[q]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GA](GA.md), [GAD](GAD.md).*)


(* ::Subsection:: *)
(*Examples*)


GS[p]


GS[p]//FCI//StandardForm


GS[p,q,r,s]


GS[p,q,r,s]//StandardForm


GS[q] . (GS[p]+m) . GS[q]
