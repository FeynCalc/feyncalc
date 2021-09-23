(* ::Package:: *)

 


(* ::Section:: *)
(*FCI*)


(* ::Text:: *)
(*`FCI[exp]` translates exp into the internal FeynCalc (datatype-)representation.*)


(* ::Text:: *)
(*`FCI` is equivalent to `FeynCalcInternal`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FeynCalcExternal](FeynCalcExternal.md), [FeynCalcInternal](FeynCalcInternal.md), [FCE](FCE.md).*)


(* ::Subsection:: *)
(*Examples*)


ex={GA[\[Mu]],GAD[\[Rho]],GS[p],SP[p,q],MT[\[Alpha],\[Beta]],FV[p,\[Mu]]}


ex//StandardForm


ex//FCI
%//StandardForm


ex//FCE
%//StandardForm



