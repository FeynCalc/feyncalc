(* ::Package:: *)

 


(* ::Section:: *)
(*CartesianPairContract*)


(* ::Text:: *)
(*`CartesianPairContract` is like `CartesianPair`, but with (local) contraction properties.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [CartesianPair](CartesianPair.md), [Contract](Contract.md).*)


(* ::Subsection:: *)
(*Examples*)


CartesianPair[CartesianIndex[i],CartesianMomentum[p]]CartesianPair[CartesianIndex[i],CartesianMomentum[q]]
%/.CartesianPair->CartesianPairContract
%/.CartesianPairContract->CartesianPair



