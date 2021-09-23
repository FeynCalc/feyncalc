(* ::Package:: *)

 


(* ::Section:: *)
(*GA*)


(* ::Text:: *)
(*`GA[mu]` can be used as input for a 4-dimensional $\gamma^{\mu }$ and is transformed into `DiracGamma[LorentzIndex[mu]]` by FeynCalcInternal (=FCI).*)


(* ::Text:: *)
(*`GA[mu , nu , ...]` is a short form for `GA[mu].GA[nu]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GAD](GAD.md), [GS](GS.md).*)


(* ::Subsection:: *)
(*Examples*)


GA[\[Mu]]


GA[\[Mu],\[Nu]]-GA[\[Nu],\[Mu]]


StandardForm[FCI[GA[\[Mu]]]]


GA[\[Mu],\[Nu],\[Rho],\[Sigma]]


StandardForm[GA[\[Mu],\[Nu],\[Rho],\[Sigma]]]


GA[\[Alpha]] . (GS[p]+m) . GA[\[Beta]]
