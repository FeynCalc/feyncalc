(* ::Package:: *)

 


(* ::Section:: *)
(*SIE*)


(* ::Text:: *)
(*`SIE[mu]` can be used as input for $D-1$-dimensional $\sigma^{\mu }$ with $D-4$-dimensional Lorentz index $\mu$ and is transformed into `PauliSigma[LorentzIndex[mu,D-4],D-4]` by FeynCalcInternal.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [SI](SI.md).*)


(* ::Subsection:: *)
(*Examples*)


SIE[\[Mu]]


SIE[\[Mu],\[Nu]]-SIE[\[Nu],\[Mu]]


StandardForm[FCI[SIE[\[Mu]]]]


SIE[\[Mu],\[Nu],\[Rho],\[Sigma]]
%//StandardForm


SIE[\[Alpha]] . (SISE[p]+m) . SIE[\[Beta]]
