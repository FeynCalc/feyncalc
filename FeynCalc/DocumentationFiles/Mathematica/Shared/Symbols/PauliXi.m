 
(* ::Section:: *)
(* PauliXi *)
(* ::Text:: *)
(*PauliXi[I] represents a two-component Pauli spinor \[Xi], while PauliXi[-I] stands for $\xi ^{\dagger }$.*)


(* ::Subsection:: *)
(* Examples *)
PauliXi[I]

PauliXi[-I]

PauliXi[-I].SIS[p].PauliEta[I]

%//ComplexConjugate
