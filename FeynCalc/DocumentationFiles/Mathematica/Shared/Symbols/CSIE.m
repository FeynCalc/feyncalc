(* ::Package:: *)

 


(* ::Section:: *)
(*CSIE*)


(* ::Text:: *)
(*`CSIE[i]` can be used as input for $D-4$-dimensional $\sigma ^i$ with $D-4$-dimensional Cartesian index `i` and is transformed into `PauliSigma[CartesianIndex[i,D-4],D-4]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[PauliSigma](PauliSigma).*)


(* ::Subsection:: *)
(*Examples*)


CSIE[i]


CSIE[i,j]-CSIE[j,i]


StandardForm[FCI[CSIE[i]]]


CSIE[i,j,k,l]


StandardForm[CSIE[i,j,k,l]]
