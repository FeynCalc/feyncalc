(* ::Package:: *)

 


(* ::Section:: *)
(*PairContract*)


(* ::Text:: *)
(*`PairContract` is like `Pair`, but with (local) contraction properties.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Pair](Pair), [Contract](Contract).*)


(* ::Subsection:: *)
(*Examples*)


Pair[LorentzIndex[\[Mu]],Momentum[p]]Pair[LorentzIndex[\[Mu]],Momentum[q]]
%/.Pair->PairContract


Pair[LorentzIndex[\[Mu]],Momentum[p]]Pair[LorentzIndex[\[Nu]],Momentum[q]]Pair[LorentzIndex[\[Mu]],LorentzIndex[\[Nu]]]
%/.Pair->PairContract



