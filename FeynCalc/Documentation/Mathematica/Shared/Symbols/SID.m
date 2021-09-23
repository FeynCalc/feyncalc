(* ::Package:: *)

 


(* ::Section:: *)
(*SID*)


(* ::Text:: *)
(*`SID[mu]`  can be used as input for $D-1$-dimensional $\sigma^{\mu }$ with $D$-dimensional Lorentz index $\mu$ and is transformed into `PauliSigma[LorentzIndex[mu,D],D-1]` by `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [SI](SI.md), [SIE](SIE.md).*)


(* ::Subsection:: *)
(*Examples*)


SID[\[Mu]]


SID[\[Mu],\[Nu]]-SID[\[Nu],\[Mu]]


StandardForm[FCI[SID[\[Mu]]]]


SID[\[Mu],\[Nu],\[Rho],\[Sigma]]
%//StandardForm


SID[\[Alpha]] . (SISD[p]+m) . SID[\[Beta]]
