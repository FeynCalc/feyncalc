 
(* ::Section:: *)
(* SID *)
(* ::Text:: *)
(*SID[mu]  can be used as input for D-1-dimensional $\sigma ^{\mu }$ with D-dimensional Lorentz index \[Mu] and is transformed into PauliSigma[LorentzIndex[mu,D],D-1] by FeynCalcInternal..*)


(* ::Subsection:: *)
(* Examples *)
SID[\[Mu]]

SID[\[Mu],\[Nu]]-SID[\[Nu],\[Mu]]

StandardForm[FCI[SID[\[Mu]]]]

SID[\[Mu],\[Nu],\[Rho],\[Sigma]]

StandardForm[SID[\[Mu],\[Nu],\[Rho],\[Sigma]]]

SID[\[Alpha]].(SISD[p]+m).SID[\[Beta]]
