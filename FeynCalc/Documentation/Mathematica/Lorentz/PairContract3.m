(* ::Package:: *)

 


(* ::Section:: *)
(*PairContract3*)


(* ::Text:: *)
(*`PairContract3` is like `Pair`, but with local contraction properties among `PairContract3`s. The function fully supports the BMHV algebra and, unlike `PairContract` or `PairContract2` will always expand momenta inside scalar products.*)


(* ::Text:: *)
(*`PairContract3` is an auxiliary function used in higher level FeynCalc functions that require fast contractions between multiple expressions, where `Contract` would be too slow.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [PairContract](PairContract.md), [PairContract2](PairContract2.md).*)


(* ::Subsection:: *)
(*Examples*)


Pair[LorentzIndex[\[Mu]],Momentum[p]]Pair[LorentzIndex[\[Mu]],Momentum[q]]

%/.Pair->PairContract3


Pair[LorentzIndex[\[Mu]],Momentum[p]]Pair[LorentzIndex[\[Nu]],Momentum[q]]Pair[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]]]

%/.Pair->PairContract3


Pair[LorentzIndex[\[Mu]],Momentum[p+q]]Pair[LorentzIndex[\[Mu]],Momentum[r+s]]

%/.Pair->PairContract3
