 
(* ::Section:: *)
(* Eps *)
(* ::Text:: *)
(*Eps[a, b, c, d] is the head of the totally antisymmetric $\epsilon$ (Levi-Civita) tensor. The a,b, ... may have head LorentzIndex, Momentum or Integer. In case of integers the Levi-Civita tensor is evaluated immediately..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*EpsContract, EpsEvaluate, LC, LCD.*)



(* ::Subsection:: *)
(* Examples *)



Eps[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]

Eps[Momentum[p],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]

Eps[b,a,c,d]//StandardForm

Eps[ExplicitLorentzIndex[0],ExplicitLorentzIndex[1],ExplicitLorentzIndex[2],ExplicitLorentzIndex[3]]

Eps[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]],LorentzIndex[\[Rho]],LorentzIndex[\[Sigma]]]

Contract[% %]

Eps[LorentzIndex[\[Mu],D],LorentzIndex[\[Nu],D],LorentzIndex[\[Rho],D],LorentzIndex[\[Sigma],D]]

Contract[% %]//Factor2

-(I/24)LCD[\[Mu],\[Nu],\[Rho],\[Alpha]].GAD[\[Mu],\[Nu],\[Rho],\[Alpha]]//FCI

-(I/24) LCD[\[Mu]^\[Prime],\[Nu]^\[Prime],\[Rho]^\[Prime],\[Alpha]^\[Prime]].GAD[\[Mu]^\[Prime],\[Nu]^\[Prime],\[Rho]^\[Prime],\[Alpha]^\[Prime]]//FCI

DiracSimplify[%.%%]//Factor2

%/.D->4
