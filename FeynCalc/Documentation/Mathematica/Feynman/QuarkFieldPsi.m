(* ::Package:: *)

 


(* ::Section:: *)
(*QuarkFieldPsi*)


(* ::Text:: *)
(*`QuarkFieldPsi` is the name of a fermionic field.This is just a name with no functional properties. Only typesetting rules are attached.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


QuarkFieldPsi


QuantumField[QuarkFieldPsiDagger] . GA[\[Mu]] . CovariantD[\[Mu]] . QuantumField[QuarkFieldPsi]
ExpandPartialD[%]
