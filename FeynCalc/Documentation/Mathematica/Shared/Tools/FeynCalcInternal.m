(* ::Package:: *)

 


(* ::Section:: *)
(*FeynCalcInternal*)


(* ::Text:: *)
(*`FeynCalcInternal[exp]` translates `exp` into the internal FeynCalc (abstract data-type) representation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FCI](FCI.md), [FCE](FCE.md).*)


(* ::Subsection:: *)
(*Examples*)


ex={GA[\[Mu]],GAD[\[Rho]],GS[p],SP[p,q],MT[\[Alpha],\[Beta]],FV[p,\[Mu]]}


ex//StandardForm


ex//FeynCalcInternal


ex//StandardForm


FeynCalcExternal[ex]//StandardForm


ex=FCI[{SD[a,b],SUND[a,b,c],SUNF[a,b,c],FAD[q],LC[\[Mu],\[Nu],\[Rho],\[Sigma]]}]


ex//StandardForm
