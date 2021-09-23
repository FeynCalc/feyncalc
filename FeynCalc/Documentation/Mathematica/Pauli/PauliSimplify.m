(* ::Package:: *)

 


(* ::Section:: *)
(*PauliSimplify*)


(* ::Text:: *)
(*`PauliSimplify[exp]` simplifies products of Pauli matrices and expands non-commutative products. Double indices and vectors are contracted. The order of the Pauli matrices is not changed.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [PauliTrick](PauliTrick.md).*)


(* ::Subsection:: *)
(*Examples*)


CSIS[p1] . CSI[i] . CSIS[p2]
PauliSimplify[%]


CSIS[p] . CSI[i,j,k] . CSIS[p]
PauliSimplify[%]


PauliSimplify[CSIS[p] . CSI[i,j,k] . CSIS[p],PauliReduce->False]


CSID[i,j,i]
PauliSimplify[%]


CSID[i,j,k,l,m,i]
PauliSimplify[%]
