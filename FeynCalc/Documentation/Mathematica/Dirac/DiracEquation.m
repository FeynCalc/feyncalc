(* ::Package:: *)

 


(* ::Section:: *)
(*DiracEquation*)


(* ::Text:: *)
(*`DiracEquation[exp]` applies the Dirac equation without expanding exp. If expansions are necessary, use `DiracSimplify`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


GS[p] . SpinorU[p,m]
DiracSimplify[%]


GS[p] . SpinorU[p,m]
DiracEquation[%]


GS[p] . SpinorV[p,m]
DiracEquation[%]


SpinorUBar[p,0] . GS[p]
DiracEquation[%]


(* ::Text:: *)
(*`DiracEquation` also works in $D$-dimensions*)


SpinorVBarD[p,m] . GSD[p]
DiracEquation[%]
