(* ::Package:: *)

 


(* ::Section:: *)
(*CGAD*)


(* ::Text:: *)
(*`CGAD[i]` can be used as input for $\gamma ^i$ in $D$ dimensions, where `i` is a Cartesian index, and is transformed into `DiracGamma[CartesianIndex[i,D-1],D]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[GAD](GAD), [DiracGamma](DiracGamma).*)


(* ::Subsection:: *)
(*Examples*)


CGAD[i]


CGAD[i,j]-CGAD[j,i]


StandardForm[FCI[CGAD[i]]]


CGAD[i,j,k,l]


StandardForm[CGAD[i,j,k,l]]


DiracSimplify[DiracTrace[CGAD[i,j,k,l]]]


CGAD[i] . (CGSD[p]+m) . CGAD[j]
