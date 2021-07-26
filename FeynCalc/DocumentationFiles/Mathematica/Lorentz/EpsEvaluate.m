(* ::Package:: *)

(* ::Section:: *)
(*EpsEvaluate*)


(* ::Text:: *)
(*`EpsEvaluate[expr]` applies total antisymmetry and linearity (w.r.t. momenta) to all Levi-Civita tensors (`Eps`) in expr.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Contract](Contract), [Eps](Eps), [LC](LC), [Trick](Trick).*)


(* ::Subsection:: *)
(*Examples*)


Contract[LC[\[Mu],\[Nu],\[Rho],\[Sigma]] FV[p+q,\[Sigma]]]//MomentumCombine
EpsEvaluate[%]
StandardForm[%]



