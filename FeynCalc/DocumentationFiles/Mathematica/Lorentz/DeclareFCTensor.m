(* ::Package:: *)

 


(* ::Section:: *)
(*DeclareFCTensor*)


(* ::Text:: *)
(*`DeclareFCTensor[a, b, ...]` declares `a`,`b`, ... to be tensor heads, i.e., `DataType[a,b, ...,  FCTensor]` is set to `True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[ExpandScalarProduct](ExpandScalarProduct), [Uncontract](Uncontract).*)


(* ::Subsection:: *)
(*Examples*)


ClearAll[myTens]
DeclareFCTensor[myTens]
ExpandScalarProduct[myTens[z,Momentum[a+b],Momentum[c+d]]]


UnDeclareFCTensor[myTens]
