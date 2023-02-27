(* ::Package:: *)

 


(* ::Section:: *)
(*FromFCPartialFractionForm*)


(* ::Text:: *)
(*`FromFCPartialFractionForm[exp]` converts all `FCPartialFractionForm` symbols present in `exp` back into the standard representation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ToFCPartialFractionForm](ToFCPartialFractionForm.md), [FCPartialFractionForm](FCPartialFractionForm.md).*)


(* ::Subsection:: *)
(*Examples*)


FromFCPartialFractionForm[FCPartialFractionForm[x,{},x]]


FromFCPartialFractionForm[FCPartialFractionForm[0,{{{x-1,-2},1}},x]]


FromFCPartialFractionForm[FCPartialFractionForm[0,{{{x+1,-1},1},{{x-y,-2},c}},x]]


FromFCPartialFractionForm[FCPartialFractionForm[0,{{{x+1,-1},1},{{x-y,-2},c}},x],Factoring->Together]
