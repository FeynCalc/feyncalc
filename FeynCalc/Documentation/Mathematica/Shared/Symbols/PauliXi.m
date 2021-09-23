(* ::Package:: *)

 


(* ::Section:: *)
(*PauliXi*)


(* ::Text:: *)
(*`PauliXi[I]` represents a two-component Pauli spinor $\xi$, while `PauliXi[-I]` stands for $\xi^{\dagger }$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliEta](PauliEta.md).*)


(* ::Subsection:: *)
(*Examples*)


PauliXi[I]


PauliXi[-I]


PauliXi[-I] . SIS[p] . PauliEta[I]
%//ComplexConjugate



