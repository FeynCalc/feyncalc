(* ::Package:: *)

 


(* ::Section:: *)
(*ThreeDivergence*)


(* ::Text:: *)
(*`ThreeDivergence[exp, CV[p, i]]`  calculates the partial derivative of `exp` w.r.t. $p^i$.*)


(* ::Text:: *)
(*` ThreeDivergence[exp, CV[p, i], CV[p,i], ...]` gives the multiple derivative.*)


(* ::Text:: *)
(*Owing to the fact that in FeynCalc dummy Cartesian index are always understood to be upper indices, applying `ThreeDivergence` to an expression is equivalent to the action of $\nabla^i = \frac{\partial}{\partial p^i}$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FourDivergence](FourDivergence.md).*)


(* ::Subsection:: *)
(*Examples*)


CSP[p,q]
ThreeDivergence[%,CV[q,i]]


CSP[p-k,q]
ThreeDivergence[%,CV[k,i]]


CFAD[{p,m^2},p-q]
ThreeDivergence[%,CVD[p,i]]



