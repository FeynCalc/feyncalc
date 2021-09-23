(* ::Package:: *)

 


(* ::Section:: *)
(*FourDivergence*)


(* ::Text:: *)
(*`FourDivergence[exp, FV[p, mu]]` calculates the partial derivative of exp w.r.t $p^{\mu }$. `FourDivergence[exp, FV[p, mu], FV[p,nu], ...]` gives the multiple derivative.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ThreeDivergence](ThreeDivergence.md).*)


(* ::Subsection:: *)
(*Examples*)


SP[p,q]
FourDivergence[%,FV[q,\[Mu]]]


SP[p-k,q]
FourDivergence[%,FV[k,\[Mu]]]


SFAD[{p,m^2}]
FourDivergence[%,FVD[p,\[Nu]]]


FVD[l, \[Mu]] FAD[{l, 0}, {l - p, 0}]
FourDivergence[%, FVD[l, \[Mu]]]


SP[p, w]*SpinorUBar[p2, m] . GS[w] . SpinorU[p1, m]
FourDivergence[%, FV[w, a]]



