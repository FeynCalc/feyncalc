(* ::Package:: *)

(* ::Section:: *)
(*Li3*)


(* ::Text:: *)
(*`Li3` is an abbreviation for the trilogarithm function, i.e. `Li3 = PolyLog[3, #]&`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Li2](Li2.md).*)


(* ::Subsection:: *)
(*Examples*)


Li3[x]


Li3//StandardForm


D[Li3[x],x]


Integrate[Li3[x]/x,x]
