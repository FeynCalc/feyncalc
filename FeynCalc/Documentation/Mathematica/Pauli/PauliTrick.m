(* ::Package:: *)

 


(* ::Section:: *)
(*PauliTrick*)


(* ::Text:: *)
(*`PauliTrick[exp]` contracts $\sigma$ matrices with each other and performs several simplifications (no expansion, use `PauliSimplify` for this).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PauliSigma](PauliSigma.md), [PauliSimplify](PauliSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


CSIS[p1] . CSI[i] . CSIS[p2]
PauliTrick[%]//Contract


CSID[i,j,i]
PauliTrick[%]//Contract


CSIS[p] . CSI[j] . CSIS[p] . CSIS[i]
PauliTrick[%]//Contract//EpsEvaluate//FCCanonicalizeDummyIndices
PauliTrick[%%, PauliReduce->False]
