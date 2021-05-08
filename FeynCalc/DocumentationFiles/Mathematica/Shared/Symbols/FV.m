 
(* ::Section:: *)
(* FV *)
(* ::Text:: *)
(*FV[p, mu] is the four-dimensional vector $p^{\mu }$..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FCE, FCI, FVD, Pair.*)



(* ::Subsection:: *)
(* Examples *)



FV[p,\[Mu]]

FV[p-q,\[Mu]]

FV[p,\[Mu]]//StandardForm

FCI[FV[p,\[Mu]]]//StandardForm


(* ::Text:: *)
(*ExpandScalarProduct is used to expand momenta in FV*)


ExpandScalarProduct[FV[p-q,\[Mu]]]
