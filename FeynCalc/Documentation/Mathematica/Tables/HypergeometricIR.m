(* ::Package:: *)

 


(* ::Section:: *)
(*HypergeometricIR*)


(* ::Text:: *)
(*`HypergeometricIR[exp, t]` substitutes for all `Hypergeometric2F1[a,b,c,z]` in `exp` by its Euler integral representation. The factor `Integratedx[t, 0, 1]` can be omitted by setting the option `Integratedx -> False`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [HypergeometricAC](HypergeometricAC.md), [HypergeometricSE](HypergeometricSE.md), [ToHypergeometric](ToHypergeometric.md).*)


(* ::Subsection:: *)
(*Examples*)


HypergeometricIR[Hypergeometric2F1[a,b,c,z],t]


ToHypergeometric[t^b (1-t)^c (1+t z)^a,t]
HypergeometricIR[%,t]



