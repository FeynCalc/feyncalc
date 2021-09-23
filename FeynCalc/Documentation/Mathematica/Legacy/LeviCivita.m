(* ::Package:: *)

 


(* ::Section:: *)
(*LeviCivita*)


(* ::Text:: *)
(*`LeviCivita[mu, nu, rho, si]` is an input function for the totally antisymmetric Levi-Civita tensor. It evaluates automatically to the internal representation `Eps[LorentzIndex[mu], LorentzIndex[nu], LorentzIndex[rho], LorentzIndex[si]]` (or with a second argument in `LorentzIndex` for the `Dimension`, if the option `Dimension` of `LeviCivita` is changed).*)


(* ::Text:: *)
(*`LeviCivita[mu , nu, ...][p, ...]` evaluates to `Eps[LorentzIndex[mu], LorentzIndex[nu], ..., Momentum[p], ...]`.*)


(* ::Text:: *)
(*The shortcut `LeviCivita` is deprecated, please use `LC` instead!*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [LC](LC.md), [FCI](FCI.md).*)


(* ::Subsection:: *)
(*Examples*)


LeviCivita[\[Alpha],\[Beta],\[Gamma],\[Delta]]


LeviCivita[][p,q,r,s]


LeviCivita[\[Alpha],\[Beta]][p,q]
StandardForm[%]


(* ::Text:: *)
(*`LeviCivita` is scheduled for removal in the future versions of FeynCalc. The safe alternative is to use `LC`.*)


LC[\[Alpha],\[Beta],\[Gamma],\[Delta]]


LC[][p,q,r,s]


LC[\[Alpha],\[Beta]][p,q]


LCD[\[Alpha],\[Beta],\[Gamma],\[Delta]]


LCD[][p,q,r,s]


LCD[\[Alpha],\[Beta]][p,q]


FCI[LC[\[Alpha],\[Beta],\[Gamma],\[Delta]]]===LeviCivita[\[Alpha],\[Beta],\[Gamma],\[Delta]]


FCI[LCD[\[Alpha],\[Beta],\[Gamma],\[Delta]]]===LeviCivita[\[Alpha],\[Beta],\[Gamma],\[Delta],Dimension->D]
