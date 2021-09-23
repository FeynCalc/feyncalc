(* ::Package:: *)

 


(* ::Section:: *)
(*EpsEvaluate*)


(* ::Text:: *)
(*`EpsEvaluate[expr]` applies total antisymmetry and linearity (w.r.t. momenta) to all Levi-Civita tensors (`Eps`) in expr.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [Eps](Eps.md), [LC](LC.md), [Trick](Trick.md).*)


(* ::Subsection:: *)
(*Examples*)


Contract[LC[\[Mu],\[Nu],\[Rho],\[Sigma]] FV[p+q,\[Sigma]]]//MomentumCombine
EpsEvaluate[%]
StandardForm[%]



