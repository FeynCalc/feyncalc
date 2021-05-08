 
(* ::Section:: *)
(* GAD *)
(* ::Text:: *)
(*GAD[\[Mu]] can be used as input for a D-dimensional $\gamma ^{\mu }{}_$and is transformed into DiracGamma[LorentzIndex[$\mu$,D],D] by FeynCalcInternal (=FCI).GAD[$\mu ,\nu , \text{...}$] is a short form for GAD[$\mu$].GAD[$\nu$]. ... ..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracGamma, GA, GS.*)



(* ::Subsection:: *)
(* Examples *)



GAD[\[Mu]]

GAD[\[Mu],\[Nu]]-GAD[\[Nu],\[Mu]]

StandardForm[FCI[GAD[\[Mu]]]]

GAD[\[Mu],\[Nu],\[Rho],\[Sigma]]

StandardForm[GAD[\[Mu],\[Nu],\[Rho],\[Sigma]]]

GAD[\[Alpha]].(GSD[p]+m).GAD[\[Beta]]
