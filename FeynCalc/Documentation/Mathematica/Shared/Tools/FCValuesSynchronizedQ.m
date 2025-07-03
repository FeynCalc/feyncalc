(* ::Package:: *)

 


(* ::Section:: *)
(*FCValuesSynchronizedQ*)


(* ::Text:: *)
(*`FCValuesSynchronizedQ[{val1, val2}, type]` compares  definitions of `val1`, `val2`, ...*)
(*between the master kernel and the subkernels and returns `True` if all of them identical. Supported `type`s are `DownValues`, `UpValues` and `OwnValues`.*)


(* ::Text:: *)
(*This routine is only relevant in the parallel mode of FeynCalc. It helps to avoid inconsistencies through definitions that were introduced before activating the parallel mode and not correctly propagated to the subkernels*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [FCScalarProductsSynchronizedQ](FCScalarProductsSynchronizedQ.md)*)


(* ::Subsection:: *)
(*Examples*)
