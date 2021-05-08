 
(* ::Section:: *)
(* FCE *)
(* ::Text:: *)
(*FCE[exp] translates exp from the internal FeynCalc representation to a short form.FCE is equivalent to FeynCalcExternal..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FeynCalcExternal, FCI, FeynCalcInternal.*)



(* ::Subsection:: *)
(* Examples *)



FCE[{DiracGamma[5],DiracGamma[Momentum[p]]}]

%//StandardForm

{GA[\[Mu]],GAD[\[Rho]],GS[p],SP[p,q],MT[\[Alpha],\[Beta]],FV[p,\[Mu]]}

%//StandardForm

%//FCI

%//StandardForm

FCE[%]//StandardForm
