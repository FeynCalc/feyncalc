 
(* ::Section:: *)
(* FeynCalcExternal *)
(* ::Text:: *)
(*FeynCalcExternal[exp] translates exp from the internal FeynCalc representation to a shorthand form..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FeynCalcInternal.*)



(* ::Subsection:: *)
(* Examples *)



FeynCalcExternal[DiracGamma[5]]

%//StandardForm

{GA[\[Mu]],GAD[\[Rho]],GS[p],SP[p,q],MT[\[Alpha],\[Beta]],FV[p,\[Mu]]}

%//StandardForm

%//FeynCalcInternal

%//StandardForm

FeynCalcExternal[%]//StandardForm
