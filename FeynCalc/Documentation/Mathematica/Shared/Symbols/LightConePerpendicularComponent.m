(* ::Package:: *)

 


(* ::Section:: *)
(*LightConePerpendicularComponent*)


(* ::Text:: *)
(*`LightConePerpendicularComponent[LorentzIndex[mu],Momentum[n],Momentum[nb]]` denotes the perpendicular component of the Lorentz index `mu` with respect to the lightcone momenta `n` and `nb`.*)


(* ::Text:: *)
(*`LightConePerpendicularComponent[Momentum[p],Momentum[n],Momentum[nb]]` denotes the perpendicular component of the 4-momentum `p` with respect to the lightcone momenta `n` and `nb`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [LorentzIndex](LorentzIndex.md), [Momentum](Momentum.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*$4$-dimensional Lorentz vector*)


Pair[LightConePerpendicularComponent[LorentzIndex[\[Mu]],Momentum[n],Momentum[nb]],
LightConePerpendicularComponent[Momentum[p],Momentum[n],Momentum[nb]]]


(* ::Text:: *)
(*Metric tensor*)


Pair[LightConePerpendicularComponent[LorentzIndex[\[Mu]],Momentum[n],Momentum[nb]],
LightConePerpendicularComponent[LorentzIndex[\[Nu]],Momentum[n],Momentum[nb]]]


(* ::Text:: *)
(*Dirac matrix*)


DiracGamma[LightConePerpendicularComponent[LorentzIndex[\[Mu]],Momentum[n],Momentum[nb]]]


(* ::Text:: *)
(*Contractions*)


DiracGamma[LightConePerpendicularComponent[LorentzIndex[\[Mu]],
Momentum[n],Momentum[nb]]]FV[p,\[Mu]]//Contract

%//StandardForm
