 
(* ::Section:: *)
(* LeftPartialD *)
(* ::Text:: *)
(*LeftPartialD[\[Mu]] denotes $\overleftarrow{\partial }_{\mu }$acting to the left..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*ExpandPartialD, FCPartialD, LeftRightPartialD, RightPartialD.*)



(* ::Subsection:: *)
(* Examples *)



QuantumField[A,LorentzIndex[\[Mu]]].LeftPartialD[\[Nu]]

ExpandPartialD[%]

StandardForm[%]

StandardForm[LeftPartialD[\[Mu]]]

QuantumField[A,LorentzIndex[\[Mu]]].QuantumField[A,LorentzIndex[\[Nu]]].LeftPartialD[\[Rho]]

ExpandPartialD[%]

StandardForm[%]
