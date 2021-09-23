(* ::Package:: *)

 


(* ::Section:: *)
(*SI*)


(* ::Text:: *)
(*`SI[mu]` can be used as input for $3$-dimensional $\sigma^{\mu }$ with 4-dimensional Lorentz index $\mu$ and is transformed into `PauliSigma[LorentzIndex[mu]]` by FeynCalcInternal.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [SID](SID.md), [SIE](SIE.md).*)


(* ::Subsection:: *)
(*Examples*)


SI[\[Mu]]


SI[\[Mu],\[Nu]]-SI[\[Nu],\[Mu]]
StandardForm[FCI[SI[\[Mu]]]]


SI[\[Mu],\[Nu],\[Rho],\[Sigma]]
%//StandardForm


SI[\[Alpha]] . (SIS[p]+m) . SI[\[Beta]]
