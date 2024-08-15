(* ::Package:: *)

 


(* ::Section:: *)
(*Eps*)


(* ::Text:: *)
(*`Eps[a, b, c, d]` is the head of the totally antisymmetric $\epsilon$ (Levi-Civita) tensor. The `a,b, ...` may have head `LorentzIndex` or `Momentum`.*)


(* ::Text:: *)
(*When some indices of a Levi-Civita-tensor are contracted with 4-vectors, FeynCalc suppresses explicit dummy indices by putting those vectors into the corresponding index slots. For example,  $\varepsilon^{p_1 p_2 p_3 p_4}$ (accessible via `LC[][p1,p2,p3,p4]`) correspond to $\varepsilon_{\mu \nu \rho \sigma} p_1^\mu p_2^\nu p_3^\rho p_4^\sigma$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [EpsContract](EpsContract.md), [EpsEvaluate](EpsEvaluate.md), [LC](LC.md), [LCD](LCD.md).*)


(* ::Subsection:: *)
(*Examples*)


Eps[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]


Eps[Momentum[p],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]


Eps[b,a,c,d]//StandardForm


Eps[ExplicitLorentzIndex[0],ExplicitLorentzIndex[1],ExplicitLorentzIndex[2],
ExplicitLorentzIndex[3]]


Eps[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]] *
Pair[LorentzIndex[\[Mu]],Momentum[Subscript[p, 1]]] Pair[LorentzIndex[\[Nu]],Momentum[Subscript[p, 2]]]*
 Pair[LorentzIndex[\[Rho]],Momentum[Subscript[p, 3]]] Pair[LorentzIndex[\[Sigma]],Momentum[Subscript[p, 4]]]

Contract[%] 


Eps[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]

Contract[% %]


Eps[LorentzIndex[\[Mu],D],LorentzIndex[\[Nu],D],LorentzIndex[\[Rho],D],LorentzIndex[\[Sigma],D]]

Contract[% %]//Factor2


ex1=-(I/24)LCD[\[Mu],\[Nu],\[Rho],\[Alpha]] . GAD[\[Mu],\[Nu],\[Rho],\[Alpha]]//FCI


ex2=-(I/24) LCD[\[Mu]',\[Nu]',\[Rho]',\[Alpha]'] . GAD[\[Mu]',\[Nu]',\[Rho]',\[Alpha]']//FCI


DiracSimplify[ex1 . ex2]//Factor2

%/.D->4
