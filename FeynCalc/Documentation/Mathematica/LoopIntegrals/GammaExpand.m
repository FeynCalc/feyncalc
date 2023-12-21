(* ::Package:: *)

 


(* ::Section:: *)
(*GammaExpand*)


(* ::Text:: *)
(*`GammaExpand[exp]` rewrites `Gamma[n + m]` in `exp` (where `n` has `Head` `Integer`).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GammaEpsilon](GammaEpsilon.md).*)


(* ::Subsection:: *)
(*Examples*)


GammaExpand[Gamma[2 + Epsilon]]


GammaExpand[Gamma[-3+Epsilon]]


GammaExpand[Gamma[1 + Epsilon]]
