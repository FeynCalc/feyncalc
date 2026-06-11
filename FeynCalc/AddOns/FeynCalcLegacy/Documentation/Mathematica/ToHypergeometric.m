(* ::Package:: *)

 


(* ::Section:: *)
(*ToHypergeometric*)


(* ::Text:: *)
(*`ToHypergeometric[t^b (1 - t)^c (1+tz)^a,t]` returns `u^a Gamma[b+1] Gamma[c+1]/Gamma[b+c+2] Hypergeometric2F1[-a,b+1,b+c+2,-z/u]`. Remember that $\textrm{Re}(b) >0$ and $\textrm{Re} (c-b) > 0$ should hold (need not be set in Mathematica).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [HypergeometricAC](HypergeometricAC.md), [HypergeometricIR](HypergeometricIR.md), [HypergeometricSE](HypergeometricSE.md).*)


(* ::Subsection:: *)
(*Examples*)


ToHypergeometric[t^b (1-t)^c (1+t z)^a,t]


ToHypergeometric[w t^(b-1) (1-t)^(c-b-1) (1-t z)^-a,t]


ToHypergeometric[t^b (1-t)^c (u+t z)^a,t]


ToHypergeometric[w t^(b-1) (1-t)^(c-b-1) (u-t z)^-a,t]
