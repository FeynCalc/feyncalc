(* ::Package:: *)

 


(* ::Section:: *)
(*PauliTrace*)


(* ::Text:: *)
(*`PauliTrace[exp]` is the head of Pauli traces. By default the trace is not evaluated. The evaluation occurs only when the option `PauliTraceEvaluate` is set to `True`. It is recommended to use `PauliSimplify`, which will automatically evaluate all Pauli traces in the input expression.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSimplify](PauliSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


PauliTrace[CSI[i,j,k,l]]


PauliTrace[CSI[i,j,k,l],PauliTraceEvaluate->True]


PauliTrace[CSI[i,j,k,l]]
%//PauliSimplify



