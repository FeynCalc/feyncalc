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
(*[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCI](FCI.md), [FeynCalcInternal](FeynCalcInternal.md).*)


(* ::Subsection:: *)
(*Examples*)


ex=FCE[{DiracGamma[5],DiracGamma[Momentum[p]]}]


ex//StandardForm


ex={GA[\[Mu]],GAD[\[Rho]],GS[p],SP[p,q],MT[\[Alpha],\[Beta]],FV[p,\[Mu]]}


ex//StandardForm


ex//FCI//StandardForm


FCE[ex]//StandardForm
