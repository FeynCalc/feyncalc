(* ::Package:: *)

(* ::Section:: *)
(*PauliReduce*)


(* ::Text:: *)
(*`PauliReduce` is an option for `PauliTrick` and other functions. It specifies whether a chain of Pauli matrices should be reduced to at most one matrix by rewriting every pair of matrices in terms of commutator and anticommutator.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[PauliTrick](PauliTrick), [PauliSimplify](PauliSimplify).*)


(* ::Subsection:: *)
(*Examples*)


CSI[i,j,k]
PauliSimplify[%]


CSI[i,j,k]
PauliSimplify[%,PauliReduce->True]



