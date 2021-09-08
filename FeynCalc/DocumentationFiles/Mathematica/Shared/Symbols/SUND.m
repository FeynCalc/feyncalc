(* ::Package:: *)

 


(* ::Section:: *)
(*SUND*)


(* ::Text:: *)
(*`SUND[a, b, c]` are the symmetric $SU(N)$ $d_{abc}$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNDelta](SUNDelta.md), [SUNF](SUNF.md), [SUNSimplify](SUNSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


SUND[a,b,c]


SUND[a,b,c,Explicit->True]


SUND[c,a,b]


SUND[a,b,b]


SUNSimplify[SUND[a,b,c] SUND[a,b,c]]


SUNSimplify[SUND[a,b,c] SUND[a,b,c],SUNNToCACF->False]//Factor2


SUNSimplify[SUND[a,b,c] SUND[e,b,c],SUNNToCACF->False]//Factor2


SUND[a,b,c]//StandardForm


SUND[a,b,c]//FCI//StandardForm


SUND[a,b,c]//FCI//FCE//StandardForm
