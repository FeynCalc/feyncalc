(* ::Package:: *)

 


(* ::Section:: *)
(*FactorList2*)


(* ::Text:: *)
(*`FactorList2[exp]` is similar to `FactorList` except that it correctly handles symbolic exponents.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Factor2](Factor2.md).*)


(* ::Subsection:: *)
(*Examples*)


FactorList2[(x[1]x[2]+x[1]x[3]+x[2]x[3])^(-3+3ep)/(x[1]^2x[2]+x[1]^2x[3])^(-1+2ep)]
