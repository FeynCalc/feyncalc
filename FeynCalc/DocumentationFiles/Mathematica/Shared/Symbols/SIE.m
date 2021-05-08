 
(* ::Section:: *)
(* SIE *)
(* ::Text:: *)
(*SIE[mu] can be used as input for D-1-dimensional $\sigma ^{\mu }$ with D-4-dimensional Lorentz index \[Mu] and is transformed into PauliSigma[LorentzIndex[mu,D-4],D-4] by FeynCalcInternal..*)


(* ::Subsection:: *)
(* Examples *)
SIE[\[Mu]]

SIE[\[Mu],\[Nu]]-SIE[\[Nu],\[Mu]]

StandardForm[FCI[SIE[\[Mu]]]]

SIE[\[Mu],\[Nu],\[Rho],\[Sigma]]

StandardForm[SIE[\[Mu],\[Nu],\[Rho],\[Sigma]]]

SIE[\[Alpha]].(SISE[p]+m).SIE[\[Beta]]
