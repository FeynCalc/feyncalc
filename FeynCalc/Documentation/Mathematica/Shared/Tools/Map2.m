(* ::Package:: *)

 


(* ::Section:: *)
(*Map2*)


(* ::Text:: *)
(*`Map2[f, exp]` is equivalent to `Map` if `Nterms[exp] > 0`, otherwise `Map2[f, exp]` gives `f[exp]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [NTerms](NTerms.md).*)


(* ::Subsection:: *)
(*Examples*)


Map2[f,a-b]


Map2[f,x]


Map2[f,{a,b,c}]


Map2[f,1]
