(* ::Package:: *)

 


(* ::Section:: *)
(*UnDeclareFCTensor*)


(* ::Text:: *)
(*`UnDeclareFCTensor[a, b, ...]` undeclares `a,b, ...` to be tensor heads, i.e., `DataType[a,b, ...,  FCTensor]` is set to `False`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DeclareFCTensor](DeclareFCTensor.md), [FCTensor](FCTensor.md).*)


(* ::Subsection:: *)
(*Examples*)


ClearAll[myTens]
DeclareFCTensor[myTens]
ExpandScalarProduct[myTens[z,Momentum[a+b],Momentum[c+d]]]


UnDeclareFCTensor[myTens]
