(* ::Package:: *)

 


(* ::Section:: *)
(*GammaExpand*)


(* ::Text:: *)
(*`GammaExpand[exp]` rewrites `Gamma[n + m]` in `exp` (where `n` has `Head` `Integer`).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[GammaEpsilon](GammaEpsilon).*)


(* ::Subsection:: *)
(*Examples*)


GammaExpand[Gamma[2 + Epsilon]]


GammaExpand[Gamma[-3+Epsilon]]


GammaExpand[Gamma[1 + Epsilon]]
