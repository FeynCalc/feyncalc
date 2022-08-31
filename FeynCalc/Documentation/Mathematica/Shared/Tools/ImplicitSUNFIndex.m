(* ::Package:: *)

 


(* ::Section:: *)
(*ImplicitSUNFIndex*)


(* ::Text:: *)
(*`ImplicitSUNFIndex` is a data type. It mainly applies to names of quantum fields specifying that the corresponding field carries an implicit $SU(N)$ index in the fundamental representation.*)


(* ::Text:: *)
(*This information can be supplied e.g. via `DataType[QuarkField, ImplicitSUNFIndex] = True`, where `QuarkField` is a possible name of the relevant field.*)


(* ::Text:: *)
(*The `ImplicitSUNFIndex` property becomes relevant when simplifying  noncommutative products involving `QuantumField`s via `ExpandPartialD`, `DotSimplify`.*)


(* ::Text:: *)
(**)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [ImplicitDiracIndex](ImplicitDiracIndex.md), [ImplicitPauliIndex](ImplicitPauliIndex.md)*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Default (possibly unwanted) behavior*)


ex=QuantumField[AntiQuarkField].SUNT[a].QuantumField[QuarkField]


DotSimplify[ex]


ExpandPartialD[ex]


(* ::Text:: *)
(*Now we let FeynCalc know that `AntiQuarkField` and `QuarkField` carry an implicit color index that connects them to the color matrix.*)


DataType[QuarkField, ImplicitSUNFIndex] = True;
DataType[AntiQuarkField, ImplicitSUNFIndex] = True;


DotSimplify[ex]


ExpandPartialD[ex]
