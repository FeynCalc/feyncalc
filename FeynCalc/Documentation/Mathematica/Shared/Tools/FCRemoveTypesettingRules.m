(* ::Package:: *)

 


(* ::Section:: *)
(*FCRemoveTypesettingRules*)


(* ::Text:: *)
(*`FCRemoveTypesettingRules[expr]` removes all typesetting rules attached to `expr`. Effectively it sets the `FormatValues` of `expr` to an empty list.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCAttachTypesettingRule](FCAttachTypesettingRule.md).*)


(* ::Subsection:: *)
(*Examples*)


ST1


FCAttachTypesettingRule[ST1,{SubscriptBox,"S","T,1"}]


ST1


FCRemoveTypesettingRules[ST1]
ST1
