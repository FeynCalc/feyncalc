(* ::Package:: *)

 


(* ::Section:: *)
(*QuarkFieldChiDagger*)


(* ::Text:: *)
(*`QuarkFieldChiDagger` is the name of a fermionic field. This is just a name with no functional properties. Only typesetting rules are attached.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


QuarkFieldChiDagger


QuantumField[QuarkFieldChiDagger] . GA[\[Mu]] . CovariantD[\[Mu]] . QuantumField[QuarkFieldChi]
ExpandPartialD[%]
