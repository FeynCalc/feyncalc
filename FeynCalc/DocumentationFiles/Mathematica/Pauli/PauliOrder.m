(* ::Package:: *)

 


(* ::Section:: *)
(*PauliOrder*)


(* ::Text:: *)
(*`PauliOrder[exp]` orders the Pauli matrices in `expr` alphabetically.*)


(* ::Text:: *)
(*`PauliOrder[exp, orderlist]` orders the Pauli matrices in expr according to `orderlist`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


CSI[k,j,i]
PauliOrder[%]


CSID[i,j,k]
PauliOrder[%]


PauliOrder[%%,{j,i,k}]
