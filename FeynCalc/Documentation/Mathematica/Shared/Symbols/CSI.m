(* ::Package:: *)

 


(* ::Section:: *)
(*CSI*)


(* ::Text:: *)
(*`CSI[i]` can be used as input for 3-dimensional $\sigma ^i$ with 3-dimensional Cartesian index `i` and is transformed into `PauliSigma[CartesianIndex[i]]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md).*)


(* ::Subsection:: *)
(*Examples*)


CSI[i]


CSI[i,j]-CSI[j,i]


StandardForm[FCI[CSI[i]]]


CSI[i,j,k,l]


StandardForm[CSI[i,j,k,l]]
