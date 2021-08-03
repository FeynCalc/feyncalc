(* ::Package:: *)

 


(* ::Section:: *)
(*CGSD*)


(* ::Text:: *)
(*`CGSD[p]` is transformed into `DiracGamma[CartesianMomentum[p, D-1], D]` by `FeynCalcInternal`.*)


(* ::Text:: *)
(*`CGSD[p,q, ...]` is equivalent to `CGSD[p].CGSD[q]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[GSD](GSD), [DiracGamma](DiracGamma).*)


(* ::Subsection:: *)
(*Examples*)


CGSD[p]


CGSD[p]//FCI//StandardForm


CGSD[p,q,r,s]


CGSD[p,q,r,s]//StandardForm


CGSD[q] . (CGSD[p]+m) . CGSD[q]
