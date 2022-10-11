(* ::Package:: *)

 


(* ::Section:: *)
(*ShiftPartialD*)


(* ::Text:: *)
(*`ShiftPartialD[exp, {derivs}, field]` uses integration-by-parts identities to shift the derivatives of `QuantumField`s such, that a term containing `derivs` acting on `field` is eliminated from the final expression.*)


(* ::Text:: *)
(*The function always assumes that the surface term vanishes*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ExplicitPartialD](ExplicitPartialD.md), [ExpandPartialD](ExpandPartialD.md), [QuantumField](QuantumField.md)*)


(* ::Subsection:: *)
(*Examples*)


exp=QuantumField[QuarkFieldPsiDagger,PauliIndex[di1]] . RightPartialD[CartesianIndex[i
]] . QuantumField[\[Phi]] . RightPartialD[CartesianIndex[j]] . QuantumField[QuarkFieldPsi,PauliIndex[di2]]


exp//ExpandPartialD


ShiftPartialD[exp,{FCPartialD[CartesianIndex[i]]},QuarkFieldPsi,FCVerbose->-1]
