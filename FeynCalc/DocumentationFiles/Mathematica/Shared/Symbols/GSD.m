(* ::Package:: *)

 


(* ::Section:: *)
(*GSD*)


(* ::Text:: *)
(*GSD[p] can be used as input for a $D$-dimensional $p^\mu \gamma_\mu$ and is transformed into `DiracGamma[Momentum[p,D],D]` by `FeynCalcInternal` (=`FCI`).*)


(* ::Text:: *)
(*`GSD[p,q, ...]` is a short form for `GSD[p].GSD[q]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GA](GA.md), [GAD](GAD.md).*)


(* ::Subsection:: *)
(*Examples*)


GSD[p]


GSD[p]//FCI//StandardForm


GSD[p,q,r,s]


GSD[p,q,r,s]//StandardForm


GSD[q] . (GSD[p]+m) . GSD[q]
