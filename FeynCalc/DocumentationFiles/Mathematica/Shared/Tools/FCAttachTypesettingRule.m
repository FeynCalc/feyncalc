 
(* ::Section:: *)
(* FCAttachTypesettingRule *)
(* ::Text:: *)
(*FCAttachTypesettingRule[expr, ...] attaches a specific TraditionalForm typesetting rule to expr. It doesn't change any properties of expr apart from adding a FormatValue with a MakeBoxes rule.Following choices are possible:FCAttachTypesettingRule[expr_, str]FCAttachTypesettingRules[expr, {SubscriptBox, var, sub}]FCAttachTypesettingRules[expr, {SuperscriptBox, var, sup}]FCAttachTypesettingRules[expr, {SubsuperscriptBox, var, sub, sup}]Use FCRemoveTypesettingRules to remove all typesetting rules attached to expr..*)


(* ::Subsection:: *)
(* Examples *)
FCRemoveTypesettingRules
mu

FCAttachTypesettingRule[mu,"\[Mu]"]
mu

mc["d_ss"]

FCAttachTypesettingRule[mc["d_ss"],{SubscriptBox,"d","ss"}]
mc["d_ss"]

m12

FCAttachTypesettingRule[m12,{SubsuperscriptBox,m,1,2}]
m12

{p1,p2,p3,p4}

MapThread[FCAttachTypesettingRule[#1,{SubscriptBox,"p",#2}]&,{{p1,p2,p3,p4},Range[4]}];
{p1,p2,p3,p4}

FCRemoveTypesettingRules[mu]
FCRemoveTypesettingRules[mc["d_ss"]]
FCRemoveTypesettingRules[m12]
FCRemoveTypesettingRules/@{p1,p2,p3,p4};
