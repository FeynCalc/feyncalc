(* ::Package:: *)

 


(* ::Section:: *)
(*FCMakeSymbols*)


(* ::Text:: *)
(*`FCMakeSymbols[name, range, type]` generates a list or a sequence of symbols (depending on the value of type) by attaching elements of the list range to name.*)


(* ::Text:: *)
(*For example, `FCMakeSymbols[mu, Range[1, 3], List]` returns `{mu1,mu2,mu3}`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCMakeIndex](FCMakeIndex.md).*)


(* ::Subsection:: *)
(*Examples*)


FCMakeSymbols[a,Range[1,4],List]


f[FCMakeSymbols[a,Range[1,4],Sequence]]


f[FCMakeSymbols[a,{1,3},Sequence]]
