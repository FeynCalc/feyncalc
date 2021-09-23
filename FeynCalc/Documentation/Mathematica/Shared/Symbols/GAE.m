(* ::Package:: *)

 


(* ::Section:: *)
(*GAE*)


(* ::Text:: *)
(*`GAE[mu]` can be used as input for a `D-4`-dimensional $\gamma^{\mu }$and is transformed into `DiracGamma[LorentzIndex[mu, D-4], D-4]` by `FeynCalcInternal` (`FCI`).*)


(* ::Text:: *)
(*`GAE[mu, nu , ...]` is a short form for `GAE[mu].GAE[nu] ...`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [GA](GA.md), [GS](GS.md), [GAD](GAD.md).*)


(* ::Subsection:: *)
(*Examples*)


GAE[\[Mu]]


GAE[\[Mu],\[Nu]]-GAE[\[Nu],\[Mu]]


StandardForm[FCI[GAE[\[Mu]]]]


GAE[\[Mu],\[Nu],\[Rho],\[Sigma]]


StandardForm[GAE[\[Mu],\[Nu],\[Rho],\[Sigma]]]


GAE[\[Alpha]] FVD[p,\[Alpha]]//Contract


GAE[\[Alpha]] FV[p,\[Alpha]]//Contract


(* ::Text:: *)
(*In order to use Dirac algebra with $D-4$-dimensional objects you need to activate the t'Hooft-Veltman-Breitenlohner-Maison scheme first*)


FCSetDiracGammaScheme["NDR"]
DiracSimplify[GAE[\[Mu]] . GAD[\[Mu]]]


FCSetDiracGammaScheme["BMHV"]
DiracSimplify[GAE[\[Mu]] . GAD[\[Mu]]]


FCSetDiracGammaScheme["NDR"]
