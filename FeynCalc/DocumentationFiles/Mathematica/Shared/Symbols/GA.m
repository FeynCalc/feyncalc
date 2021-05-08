 
(* ::Section:: *)
(* GA *)
(* ::Text:: *)
(*GA[\[Mu]] can be used as input for a 4-dimensional $\gamma ^{\mu }{}_$and is transformed into DiracGamma[LorentzIndex[$\mu$]] by FeynCalcInternal (=FCI).GA[$\mu ,\nu ,$ ...] is a short form for GA[$\mu$].GA[$\nu$]..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracGamma, GAD, GS.*)



(* ::Subsection:: *)
(* Examples *)



GA[\[Mu]]

GA[\[Mu],\[Nu]]-GA[\[Nu],\[Mu]]

StandardForm[FCI[GA[\[Mu]]]]

GA[\[Mu],\[Nu],\[Rho],\[Sigma]]

StandardForm[GA[\[Mu],\[Nu],\[Rho],\[Sigma]]]

GA[\[Alpha]].(GS[p]+m).GA[\[Beta]]
