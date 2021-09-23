(* ::Package:: *)

 


(* ::Section:: *)
(*UnDeclareNonCommutative*)


(* ::Text:: *)
(*`UnDeclareNonCommutative[a, b, ...]` undeclares `a,b, ...` to be noncommutative, i.e., `DataType[a,b, ..., NonCommutative]` is set to `False`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [DeclareNonCommutative](DeclareNonCommutative.md).*)


(* ::Subsection:: *)
(*Examples*)


DeclareNonCommutative[x]


(* ::Text:: *)
(*As a side-effect of DeclareNonCommutative x is declared to be of DataType NonCommutative.*)


DataType[x,NonCommutative]


(* ::Text:: *)
(*The inverse operation is UnDeclareNonCommutative.*)


UnDeclareNonCommutative[x]
DataType[x,NonCommutative]


DeclareNonCommutative[y,z]
DataType[y,z,NonCommutative]



UnDeclareNonCommutative[y,z]
DataType[y,z,NonCommutative]
