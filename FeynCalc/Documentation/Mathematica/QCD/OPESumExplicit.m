(* ::Package:: *)

 


(* ::Section:: *)
(*OPESumExplicit*)


(* ::Text:: *)
(*`OPESumExplicit[exp]` calculates `OPESum`s.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [OPESum](OPESum.md), [OPESumSimplify](OPESumSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


OPESum[A^iB^(m-i-3),{i,0,m-3}]
OPESumExplicit[%]


OPESum[a^ib^(j-i)c^(m-j-4),{i,0,j},{j,0,m-4}]
OPESumExplicit[%]



