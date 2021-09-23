(* ::Package:: *)

 


(* ::Section:: *)
(*HypergeometricAC*)


(* ::Text:: *)
(*`HypergeometricAC[n][exp]` analytically continues `Hypergeometric2F1` functions in `exp`. The second argument `n` refers to the equation number ($n$) in chapter 2.10 of "Higher Transcendental Functions" by Erdelyi, Magnus, Oberhettinger, Tricomi. In case of eq. (6) (p.109) the last line is returned for `HypergeometricAC[6][exp]`, while the first equality is given by `HypergeometricAC[61][exp]`.*)


(* ::Text:: *)
(*(2.10.1) is identical to eq. (9.5.7) of "Special Functions & their Applications" by N.N.Lebedev.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [HypExplicit](HypExplicit.md), [HypergeometricIR](HypergeometricIR.md), [HypergeometricSE](HypergeometricSE.md), [ToHypergeometric](ToHypergeometric.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*These are all transformation rules currently built in.*)


HypergeometricAC[1][Hypergeometric2F1[\[Alpha],\[Beta],\[Gamma],z]]


HypergeometricAC[2][Hypergeometric2F1[\[Alpha],\[Beta],\[Gamma],z]]


HypergeometricAC[3][Hypergeometric2F1[\[Alpha],\[Beta],\[Gamma],z]]


HypergeometricAC[4][Hypergeometric2F1[\[Alpha],\[Beta],\[Gamma],z]]


HypergeometricAC[6][Hypergeometric2F1[\[Alpha],\[Beta],\[Gamma],z]]


HypergeometricAC[61][Hypergeometric2F1[\[Alpha],\[Beta],\[Gamma],z]]
