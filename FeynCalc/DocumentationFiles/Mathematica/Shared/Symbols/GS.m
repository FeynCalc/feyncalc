 
(* ::Section:: *)
(* GS *)
(* ::Text:: *)
(*GS[p] can be used as input for a 4-dimensional $p\text{ $\nmedspace $}\nmedspace \nmedspace \nmedspace \left/ \left(=\gamma .p = \gamma _{\mu }p^{\mu }\right)\right.$ and is transformed into DiracGamma[Momentum[p]] by FeynCalcInternal (=FCI). GS[p,q, ...] is a short form for GS[p].GS[q]. ... ..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*DiracGamma, GA, GAD.*)



(* ::Subsection:: *)
(* Examples *)



GS[p]

GS[p]//FCI//StandardForm

GS[p,q,r,s]

GS[p,q,r,s]//StandardForm

GS[q].(GS[p]+m).GS[q]
