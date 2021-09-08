(* ::Package:: *)

 


(* ::Section:: *)
(*Eps*)


(* ::Text:: *)
(*`Eps[a, b, c, d]` is the head of the totally antisymmetric $\epsilon$ (Levi-Civita) tensor. The `a,b, ...` may have head `LorentzIndex` or `Momentum`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [EpsContract](EpsContract.md), [EpsEvaluate](EpsEvaluate.md), [LC](LC.md), [LCD](LCD.md).*)


(* ::Subsection:: *)
(*Examples*)


Eps[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]


Eps[Momentum[p],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]


Eps[b,a,c,d]//StandardForm


Eps[ExplicitLorentzIndex[0],ExplicitLorentzIndex[1],ExplicitLorentzIndex[2],ExplicitLorentzIndex[3]]


Eps[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]
Contract[% %]


Eps[LorentzIndex[\[Mu],D],LorentzIndex[\[Nu],D],LorentzIndex[\[Rho],D],LorentzIndex[\[Sigma],D]]
Contract[% %]//Factor2


ex1=-(I/24)LCD[\[Mu],\[Nu],\[Rho],\[Alpha]] . GAD[\[Mu],\[Nu],\[Rho],\[Alpha]]//FCI


ex2=-(I/24) LCD[\[Mu]^\[Prime],\[Nu]^\[Prime],\[Rho]^\[Prime],\[Alpha]^\[Prime]] . GAD[\[Mu]^\[Prime],\[Nu]^\[Prime],\[Rho]^\[Prime],\[Alpha]^\[Prime]]//FCI


DiracSimplify[ex1 . ex2]//Factor2
%/.D->4



