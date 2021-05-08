 
(* ::Section:: *)
(* PauliEta *)
(* ::Text:: *)
(*PauliEta[I] represents a two-component Pauli spinor \[Eta], while PauliEta[-I] stands for $\eta ^{\dagger }$.*)


(* ::Subsection:: *)
(* Examples *)
PauliEta[I]

PauliEta[-I]

PauliEta[-I].SIS[p].PauliXi[I]

%//ComplexConjugate
