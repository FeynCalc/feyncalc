(* ::Package:: *)

 


(* ::Section:: *)
(*FCE*)


(* ::Text:: *)
(*`FCE[exp]` translates `exp` from the internal FeynCalc representation to a short form.*)


(* ::Text:: *)
(*`FCE` is equivalent to `FeynCalcExternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FeynCalcExternal](FeynCalcExternal), [FCI](FCI), [FeynCalcInternal](FeynCalcInternal).*)


(* ::Subsection:: *)
(*Examples*)


FCE[{DiracGamma[5],DiracGamma[Momentum[p]]}]
%//StandardForm


ex={GA[\[Mu]],GAD[\[Rho]],GS[p],SP[p,q],MT[\[Alpha],\[Beta]],FV[p,\[Mu]]}
%//StandardForm


ex//FCI
%//StandardForm


FCE[ex]//StandardForm
