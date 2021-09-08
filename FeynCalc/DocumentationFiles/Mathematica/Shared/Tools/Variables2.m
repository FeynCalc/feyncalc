(* ::Package:: *)

 


(* ::Section:: *)
(*Variables2*)


(* ::Text:: *)
(*`Variables2[expr]` is like `Variables`, but it also works on rules and equalities as well as lists thereof.*)


(* ::Text:: *)
(*`Variables2` always applies `Union` to the output.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Cases2](Cases2.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Some cases where `Variables2` is much more useful than `Variables`*)


Variables[{a->x1+y1,b->x2+y2}]


Variables2[{a->x1+y1,b->x2+y2}]


Variables[a+b==c+d]


Variables2[a+b==c+d]
