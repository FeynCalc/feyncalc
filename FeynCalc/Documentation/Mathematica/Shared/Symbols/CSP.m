(* ::Package:: *)

 


(* ::Section:: *)
(*CSP*)


(* ::Text:: *)
(*`CSP[p, q]` is the 3-dimensional scalar product of `p` with `q` and is transformed into `CartesianPair[CartesianMomentum[p],CartesianMomentum[q]]` by `FeynCalcInternal`.*)


(* ::Text:: *)
(*` CSP[p]` is the same as `CSP[p,p]` ($=p^2$).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SP](SP.md), [ScalarProduct](ScalarProduct.md), [CartesianScalarProduct](CartesianScalarProduct.md).*)


(* ::Subsection:: *)
(*Examples*)


CSP[p,q] + CSP[q]


CSP[p-q,q+2p]


Calc[ CSP[p-q,q+2p] ]


ExpandScalarProduct[CSP[p-q]]


CSP[a,b]//StandardForm


CSP[a,b]//FCI//StandardForm


CSP[a,b]//FCI//FCE//StandardForm
