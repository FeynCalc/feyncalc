(* ::Package:: *)

 


(* ::Section:: *)
(*$ParallelizeFeynCalc*)


(* ::Text:: *)
(*`$ParallelizeFeynCalc` is a global switch that enables FeynCalc to evaluate some subroutines on using parallel kernels. It should be explicitly activated by setting `$ParallelizeFeynCalc` to `True`. However, before that one should evaluate `LaunchKernels[n]` with `n` being the number of parallel kernels to launch. The default value is `False`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)


(* ::Subsection:: *)
(*Examples*)


$ParallelizeFeynCalc
