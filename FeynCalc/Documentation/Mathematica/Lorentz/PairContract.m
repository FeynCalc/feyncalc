(* ::Package:: *)

 


(* ::Section:: *)
(*PairContract*)


(* ::Text:: *)
(*`PairContract` is like `Pair`, but with (local) contraction properties. The function fully supports the BMHV algebra and will expand momenta inside scalar products when it leads to simpler expressions.*)


(* ::Text:: *)
(*`PairContract` is an auxiliary function used in higher level FeynCalc functions that require fast contractions between multiple expressions, where `Contract` would be too slow.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Contract](Contract.md), [PairContract2](PairContract2.md), [PairContract3](PairContract3.md).*)


(* ::Subsection:: *)
(*Examples*)


Pair[LorentzIndex[\[Mu]],Momentum[p]]Pair[LorentzIndex[\[Mu]],Momentum[q]]

%/.Pair->PairContract


Pair[LorentzIndex[\[Mu]],Momentum[p]]Pair[LorentzIndex[\[Nu]],Momentum[q]]Pair[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]]]

%/.Pair->PairContract


Pair[LorentzIndex[\[Mu]],Momentum[p+q]]Pair[LorentzIndex[\[Mu]],Momentum[r+s]]

%/.Pair->PairContract


FCClearScalarProducts[];
SP[p1, p2] = s2;
SP[p1, p3] = s3;
FCI[SP[p1, p2 + p3]] /. Pair -> PairContract
