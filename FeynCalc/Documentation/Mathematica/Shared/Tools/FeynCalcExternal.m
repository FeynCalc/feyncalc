(* ::Package:: *)

 


(* ::Section:: *)
(*FeynCalcExternal*)


(* ::Text:: *)
(*`FeynCalcExternal[exp]` translates exp from the internal FeynCalc representation to a shorthand form.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynCalcInternal](FeynCalcInternal.md).*)


(* ::Subsection:: *)
(*Examples*)


FeynCalcExternal[DiracGamma[5]]
%//StandardForm


ex={GA[\[Mu]],GAD[\[Rho]],GS[p],SP[p,q],MT[\[Alpha],\[Beta]],FV[p,\[Mu]]}
%//StandardForm


ex//FeynCalcInternal
%//StandardForm
FeynCalcExternal[%]//StandardForm



