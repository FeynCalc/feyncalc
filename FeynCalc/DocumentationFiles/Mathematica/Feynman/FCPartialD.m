 
(* ::Section:: *)
(* FCPartialD *)
(* ::Text:: *)
(*FCPartialD[mu] denotes the four-dimensional $\partial _{\mu }.$ FCPartialD is used to denote derivative fields. FCPartialD[LorentzIndex[$\mu ,D$]] denotes the $\text{D}$-dimensional $\partial _{\mu }.$.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*ExpandPartialD, LeftPartialD, LeftRightPartialD, RightPartialD.*)



(* ::Subsection:: *)
(* Examples *)



QuantumField[A,{\[Mu]}].LeftPartialD[\[Nu]]

ExpandPartialD[%]

StandardForm[%]
