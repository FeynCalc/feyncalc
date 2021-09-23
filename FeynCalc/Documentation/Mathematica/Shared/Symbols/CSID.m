(* ::Package:: *)

 


(* ::Section:: *)
(*CSID*)


(* ::Text:: *)
(*`CSID[i]` can be used as input for $D-1$-dimensional $\sigma^i$ with $D-1$-dimensional Cartesian index `i` and is transformed into `PauliSigma[CartesianIndex[i,D-1],D-1]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).*)


(* ::Subsection:: *)
(*Examples*)


CSID[i]


CSID[i,j]-CSID[j,i]


StandardForm[FCI[CSID[i]]]


CSID[i,j,k,l]


StandardForm[CSID[i,j,k,l]]
