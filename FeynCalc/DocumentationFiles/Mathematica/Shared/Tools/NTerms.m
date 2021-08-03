(* ::Package:: *)

 


(* ::Section:: *)
(*NTerms*)


(* ::Text:: *)
(*`NTerms[x]` is equivalent to `Length` if `x` is a sum; otherwise `NTerms[x]` returns `1`, except `NTerms[0] -> 0`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Subsection:: *)
(*Examples*)


NTerms[a-b]


NTerms[a b c]


NTerms[9]


NTerms[0]
