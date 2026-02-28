(* ::Package:: *)

(* ::Section:: *)
(*FCLoopCreateFactorizingRules*)


(* ::Text:: *)
(*`FCLoopCreateFactorizingRules[ints, topos]` processes the given list of GLIs and corresponding topologies and returns a list of rules for replacing all factorizing integrals by simpler integrals with less loops.*)


(* ::Text:: *)
(*Notice that we automatically generate suitable `FCTopology` objects for the simpler integrals. Using the options `PreferredTopologies` or `PreferredIntegrals` those can be mapped to a desired set.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopFactorizingQ](FCLoopFactorizingQ.md), [FCLoopCreateFactorizingRules](FCLoopCreateFactorizingRules.md).*)


(* ::Subsection:: *)
(*Examples*)


masters=Get@FileNameJoin[{$FeynCalcDirectory,"Documentation","Examples","MasterIntegrals",
"mastersBMixing3L.m"}];


topos=Get@FileNameJoin[{$FeynCalcDirectory,"Documentation","Examples","MasterIntegrals",
"toposBMixing3L.m"}];


FCLoopCreateFactorizingRules[masters[[1;;50]],topos]



