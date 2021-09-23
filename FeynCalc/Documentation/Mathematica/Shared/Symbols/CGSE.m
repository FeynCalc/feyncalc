(* ::Package:: *)

 


(* ::Section:: *)
(*CGSE*)


(* ::Text:: *)
(*`CGSE[p]` is transformed into `DiracGamma[CartesianMomentum[p, D-4], D-4]` by FeynCalcInternal.*)


(* ::Text:: *)
(*`CGSE[p,q, ...]` is equivalent to `CGSE[p].CGSE[q]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GSE](GSE.md), [DiracGamma](DiracGamma.md).*)


(* ::Subsection:: *)
(*Examples*)


CGSE[p]


CGSE[p]//FCI//StandardForm


CGSE[p,q,r,s]


CGSE[p,q,r,s]//StandardForm


CGSE[q] . (CGSE[p]+m) . CGSE[q]
