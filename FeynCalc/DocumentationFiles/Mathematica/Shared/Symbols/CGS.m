 
(* ::Section:: *)
(* CGS *)
(* ::Text:: *)
(*CGS[p] is transformed into DiracGamma[CartesianMomentum[p]] by FeynCalcInternal. CGS[p,q, ...] is equivalent to CGS[p].CGS[q]. ....*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*GS, DiracGamma.*)



(* ::Subsection:: *)
(* Examples *)



CGS[p]

CGS[p]//FCI//StandardForm

CGS[p,q,r,s]

CGS[p,q,r,s]//StandardForm

CGS[q].(CGS[p]+m).CGS[q]
