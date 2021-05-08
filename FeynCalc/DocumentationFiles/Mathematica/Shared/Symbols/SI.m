 
(* ::Section:: *)
(* SI *)
(* ::Text:: *)
(*SI[mu] can be used as input for 3-dimensional $\sigma ^{\mu }$ with 4-dimensional Lorentz index \[Mu] and is transformed into PauliSigma[LorentzIndex[mu]] by FeynCalcInternal..*)


(* ::Subsection:: *)
(* Examples *)
SI[\[Mu]]

SI[\[Mu],\[Nu]]-SI[\[Nu],\[Mu]]

StandardForm[FCI[SI[\[Mu]]]]

SI[\[Mu],\[Nu],\[Rho],\[Sigma]]

StandardForm[SI[\[Mu],\[Nu],\[Rho],\[Sigma]]]

SI[\[Alpha]].(SIS[p]+m).SI[\[Beta]]
