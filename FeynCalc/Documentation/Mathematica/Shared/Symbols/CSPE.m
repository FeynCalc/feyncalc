(* ::Package:: *)

 


(* ::Section:: *)
(*CSPE*)


(* ::Text:: *)
(*`CSPE[p, q]` is the $D-4$-dimensional scalar product of `p` with `q` and is transformed into `CartesianPair[CartesianMomentum[p, D-4],CartesianMomentum[q, D-4]]` by `FeynCalcInternal`.*)


(* ::Text:: *)
(*` CSPE[p]` is the same as `CSPE[p,p]` ( $=p^2$).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SPE](SPE.md), [ScalarProduct](ScalarProduct.md), [CartesianScalarProduct](CartesianScalarProduct.md).*)


(* ::Subsection:: *)
(*Examples*)


CSPE[p,q] + CSPE[q]


CSPE[p-q,q+2p]


Calc[ CSPE[p-q,q+2p] ]


ExpandScalarProduct[CSPE[p-q]]


CSPE[a,b]//StandardForm


CSPE[a,b]//FCI//StandardForm


CSPE[a,b]//FCI//FCE//StandardForm


FCE[ChangeDimension[CSP[p,q], D-4]]//StandardForm
