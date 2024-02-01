(* ::Package:: *)

 


(* ::Section:: *)
(*CartesianPairContract*)


(* ::Text:: *)
(*`CartesianPairContract` is like `CartesianPair`, but with (local) contraction properties.  The function fully supports the BMHV algebra and will not expand momenta inside scalar products.*)


(* ::Text:: *)
(*`CartesianPairContract` is an auxiliary function used in higher level FeynCalc functions that require fast contractions between multiple expressions, where `Contract` would be too slow.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [CartesianPair](CartesianPair.md), [Contract](Contract.md).*)


(* ::Subsection:: *)
(*Examples*)


CartesianPair[CartesianIndex[i],CartesianMomentum[p]]CartesianPair[CartesianIndex[i],CartesianMomentum[q]]

%/.CartesianPair->CartesianPairContract

%/.CartesianPairContract->CartesianPair


CartesianPair[CartesianIndex[i],CartesianMomentum[p]]CartesianPair[CartesianIndex[j],CartesianMomentum[q]]CartesianPair[CartesianIndex[i],CartesianIndex[j]]

%/.CartesianPair->CartesianPairContract

%/.CartesianPairContract->CartesianPair


CartesianPair[CartesianIndex[i],CartesianMomentum[p+q]]CartesianPair[CartesianIndex[i],CartesianMomentum[r+s]]

%/.CartesianPair->CartesianPairContract

%/.CartesianPairContract->CartesianPair
