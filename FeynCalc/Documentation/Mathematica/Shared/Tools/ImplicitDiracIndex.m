(* ::Package:: *)

 


(* ::Section:: *)
(*ImplicitDiracIndex*)


(* ::Text:: *)
(*`ImplicitDiracIndex` is a data type. It mainly applies to names of quantum fields specifying that the corresponding field carries an implicit Dirac index.*)


(* ::Text:: *)
(*This information can be supplied e.g. via `DataType[QuarkField, ImplicitDiracIndex] = True`, where `QuarkField` is a possible name of the relevant field.*)


(* ::Text:: *)
(*The `ImplicitDiracIndex` property becomes relevant when simplifying  noncommutative products involving `QuantumField`s via `ExpandPartialD`, `DotSimplify`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [ImplicitSUNFIndex](ImplicitSUNFIndex.md), [ImplicitPauliIndex](ImplicitPauliIndex.md)*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Default (possibly unwanted) behavior*)


ex=QuantumField[AntiQuarkField] . GA[\[Mu]] . QuantumField[QuarkField]


ExpandPartialD[ex]


(* ::Text:: *)
(*Now we let FeynCalc know that `AntiQuarkField` and `QuarkField` carry an implicit Dirac index that connects them to the Dirac matrix.*)


DataType[QuarkField, ImplicitDiracIndex] = True;
DataType[AntiQuarkField, ImplicitDiracIndex] = True;


ExpandPartialD[ex]
