(* ::Package:: *)

 


(* ::Section:: *)
(*ImplicitPauliIndex*)


(* ::Text:: *)
(*`ImplicitPauliIndex` is a data type. It mainly applies to names of quantum fields specifying that the corresponding field carries an implicit Pauli index.*)


(* ::Text:: *)
(*This information can be supplied e.g. via `DataType[QuarkFieldChi, ImplicitPauliIndex] = True`, where `QuarkFieldChi` is a possible name of the relevant field.*)


(* ::Text:: *)
(*The `ImplicitDiracIndex` property becomes relevant when simplifying  noncommutative products involving `QuantumField`s via `ExpandPartialD`, `DotSimplify`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [ImplicitSUNFIndex](ImplicitSUNFIndex.md), [ImplicitDiracIndex](ImplicitDiracIndex.md)*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Default (possibly unwanted) behavior*)


ex=QuantumField[QuarkFieldChiDagger].CSI[i].QuantumField[QuarkFieldChi]


ExpandPartialD[ex]


(* ::Text:: *)
(*Now we let FeynCalc know that `QuarkFieldChiDagger` and `QuarkFieldChi` carry an implicit Pauli index that connects them to the Pauli matrix.*)


DataType[QuarkFieldChi, ImplicitPauliIndex] = True;
DataType[QuarkFieldChiDagger, ImplicitPauliIndex] = True;


ExpandPartialD[ex]
