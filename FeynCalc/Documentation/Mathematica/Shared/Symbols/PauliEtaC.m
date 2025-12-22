(* ::Package:: *)

 


(* ::Section:: *)
(*PauliEtaC*)


(* ::Text:: *)
(*`PauliEta[I]` represents a two-component Pauli spinor $\eta_C$, while `PauliEtaC[-I]` stands for $\eta_C^{\dagger }$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliXi](PauliXi.md).*)


(* ::Subsection:: *)
(*Examples*)


PauliEtaC[I]


PauliEtaC[-I]


PauliEtaC[-I] . SIS[p] . PauliXi[I]

%//ComplexConjugate



