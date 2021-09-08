(* ::Package:: *)

 


(* ::Section:: *)
(*PauliEta*)


(* ::Text:: *)
(*`PauliEta[I]` represents a two-component Pauli spinor `\eta`, while `PauliEta[-I]` stands for $\eta^{\dagger }$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliXi](PauliXi.md).*)


(* ::Subsection:: *)
(*Examples*)


PauliEta[I]


PauliEta[-I]


PauliEta[-I] . SIS[p] . PauliXi[I]
%//ComplexConjugate
