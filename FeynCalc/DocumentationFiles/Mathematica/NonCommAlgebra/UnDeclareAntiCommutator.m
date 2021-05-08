 
(* ::Section:: *)
(* UnDeclareAntiCommutator *)
(* ::Text:: *)
(*UnDeclareAntiCommutator[a, b] undeclares the value assigned to the anticommutator of a and b..*)


(* ::Subsection:: *)
(* Examples *)
AntiCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]],A],QuantumField[A]]=0;
QuantumField[A].QuantumField[A].LeftPartialD[\[Nu]]

ExpandPartialD[%]

UnDeclareAntiCommutator[QuantumField[FCPartialD[LorentzIndex[xxx_]],A],QuantumField[A]];
ExpandPartialD[QuantumField[A].QuantumField[A].LeftPartialD[\[Nu]]]
