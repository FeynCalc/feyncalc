(* ::Package:: *)

 


(* ::Section:: *)
(*FCPatternFreeQ*)


(* ::Text:: *)
(*`FCPatternFreeQ[{exp}]` yields `True` if `{exp}` does not contain any pattern objects, e.g. `Pattern`, `Blank`, `BlankSequence` and `BlankNullSequence`.*)


(* ::Text:: *)
(*`FCPatternFreeQ[{exp},{h1,h2,...}]` checks that in addition to the pattern objects, no heads `h1, h2, ...` are present.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md).*)


(* ::Subsection:: *)
(*Examples*)


FCPatternFreeQ[{a}]


FCPatternFreeQ[{a_}]


FCPatternFreeQ[{g[x]},{g}]
