(* ::Package:: *)

 


(* ::Section:: *)
(*CSPD*)


(* ::Text:: *)
(*`CSPD[p, q]` is the $D-1$-dimensional scalar product of `p` with `q` and is transformed into `CartesianPair[CartesianMomentum[p, D-1],CartesianMomentum[q, D-1]]` by `FeynCalcInternal`.*)


(* ::Text:: *)
(*`CSPD[p]` is the same as `CSPD[p,p]` ($=p^2$).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SPD](SPD.md), [ScalarProduct](ScalarProduct.md), [CartesianScalarProduct](CartesianScalarProduct.md).*)


(* ::Subsection:: *)
(*Examples*)


CSPD[p,q] + CSPD[q]


CSPD[p-q,q+2p]


Calc[ CSPD[p-q,q+2p] ]


ExpandScalarProduct[CSPD[p-q]]


CSPD[a,b]//StandardForm


CSPD[a,b]//FCI//StandardForm


CSPD[a,b]//FCI//FCE//StandardForm


FCE[ChangeDimension[CSP[p,q], D]]//StandardForm
