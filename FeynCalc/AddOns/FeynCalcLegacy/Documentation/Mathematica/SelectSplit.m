(* ::Package:: *)

 


(* ::Section:: *)
(*SelectSplit*)


(* ::Text:: *)
(*`SelectSplit[l, p]` constructs list of mutually exclusive subsets from `l` in which every element `li` satisfies a criterion `pj[li]` with `pj` from `p` and appends the subset of remaining unmatched elements.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SelectFree](SelectFree.md).*)


(* ::Subsection:: *)
(*Examples*)


SelectSplit[{a^2,b^3,c^4,d^5,e^6,f+g,h^4},{MatchQ[#,_^2]&,MatchQ[#,_^4]&,FreeQ[#,Power]&}]


SelectSplit[{a^2,b^3,c^4,d^5,e^6,f+g,h^4},{FreeQ[#,Plus]&,FreeQ[#,Power]&}]
