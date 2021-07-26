(* ::Package:: *)

(* ::Section:: *)
(*CartesianPairContract*)


(* ::Text:: *)
(*`CartesianPairContract` is like `CartesianPair`, but with (local) contraction properties.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[CartesianPair](CartesianPair), [Contract](Contract).*)


(* ::Subsection:: *)
(*Examples*)


CartesianPair[CartesianIndex[i],CartesianMomentum[p]]CartesianPair[CartesianIndex[i],CartesianMomentum[q]]
%/.CartesianPair->CartesianPairContract
%/.CartesianPairContract->CartesianPair



