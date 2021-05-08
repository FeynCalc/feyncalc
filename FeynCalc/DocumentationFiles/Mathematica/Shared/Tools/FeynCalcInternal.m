 
(* ::Section:: *)
(* FeynCalcInternal *)
(* ::Text:: *)
(*FeynCalcInternal[exp] translates exp into the internal FeynCalc (abstract data-type) representation..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FeynCalcExternal, FCI, FCE.*)



(* ::Subsection:: *)
(* Examples *)



{GA[\[Mu]],GAD[\[Rho]],GS[p],SP[p,q],MT[\[Alpha],\[Beta]],FV[p,\[Mu]]}

%//StandardForm

%//FeynCalcInternal

%//StandardForm

FeynCalcExternal[%]//StandardForm

FCI[{SD[a,b],SUND[a,b,c],SUNF[a,b,c],FAD[q],LC[\[Mu],\[Nu],\[Rho],\[Sigma]]}]

%//StandardForm
