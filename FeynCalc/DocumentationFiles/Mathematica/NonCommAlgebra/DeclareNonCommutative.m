(* ::Package:: *)

 


(* ::Section:: *)
(*DeclareNonCommutative*)


(* ::Text:: *)
(*`DeclareNonCommutative[a, b, ...]` declares `a,b, ...` to be non-commutative, i.e., `DataType[a,b, ..., NonCommutative]` is set to `True`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[DataType](DataType), [UnDeclareNonCommutative](UnDeclareNonCommutative).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*As a side effect of `DeclareNonCommutative`, `x` is declared to be of data type `NonCommutative`.*)


DeclareNonCommutative[x]


DataType[x,NonCommutative]


DeclareNonCommutative[y,z]
DataType[a,x,y,z,NonCommutative]


UnDeclareNonCommutative[x,y,z]
DataType[a,x,y,z,NonCommutative]
