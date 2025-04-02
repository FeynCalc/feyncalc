(* ::Package:: *)

(* ::Section:: *)
(*AugmentedTopologyMarker*)


(* ::Text:: *)
(*`AugmentedTopologyMarker` is an option for `FCLoopTensorReduce`, `FCLoopAugmentTopology` and other functions. It marks topologies (written as GLIs denoting denominators only) that need to be augmented with propagators containing the given external momenta. This case usually occurs when doing a tensor reduction with zero Gram determinant*)
(*and light-like external momenta, so that an auxiliary vector must be added to the*)
(*basis.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopTensorReduce](FCLoopTensorReduce.md), [FCLoopAugmentTopology](FCLoopAugmentTopology.md).*)


(* ::Subsection:: *)
(*Examples*)
