(* ::Package:: *)

 


(* ::Section:: *)
(*FourLaplacian*)


(* ::Text:: *)
(*`FourLaplacian[exp, p, q]` is $\frac{\partial}{\partial p_{\mu }} \frac{\partial}{\partial q_{\mu }}$ applied to `exp`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FourDivergence](FourDivergence.md), [ThreeDivergence](ThreeDivergence.md).*)


(* ::Subsection:: *)
(*Examples*)


SP[q,q]
FourLaplacian[%,q,q]


SOD[q]^OPEmFAD[q,q-p]
FourLaplacian[%,q, q]



