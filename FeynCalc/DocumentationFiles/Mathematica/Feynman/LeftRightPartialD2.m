 
(* ::Section:: *)
(* LeftRightPartialD2 *)
(* ::Text:: *)
(*LeftRightPartialD2[mu] denotes $\overleftrightarrow{\partial }_{\mu }$, acting to the left and right. ExplicitPartialD[LeftRightPartialD2[$\mu$]] gives (RightPartialD[$\mu$] + LeftPartialD[$\mu$])..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*ExplicitPartialD, ExpandPartialD, FCPartialD, LeftPartialD, RightPartialD.*)



(* ::Subsection:: *)
(* Examples *)



LeftRightPartialD2[\[Mu]]

ExplicitPartialD[%]

LeftRightPartialD2[\[Mu]].QuantumField[A,LorentzIndex[\[Nu]]]

ExpandPartialD[%]

QuantumField[A,LorentzIndex[\[Mu]]].LeftRightPartialD2[\[Nu]].QuantumField[A,LorentzIndex[\[Rho]]]

ExpandPartialD[%]
