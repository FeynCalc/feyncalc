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
(*[FeynCalcExternal](FeynCalcExternal), [FeynCalcInternal](FeynCalcInternal), [FCE](FCE).*)


(* ::Subsection:: *)
(*Examples*)


ex={GA[\[Mu]],GAD[\[Rho]],GS[p],SP[p,q],MT[\[Alpha],\[Beta]],FV[p,\[Mu]]}


ex//StandardForm


ex//FCI
%//StandardForm


ex//FCE
%//StandardForm



