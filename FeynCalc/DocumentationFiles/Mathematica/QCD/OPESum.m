(* ::Package:: *)

 


(* ::Section:: *)
(*OPESum*)


(* ::Text:: *)
(*`OPESum[exp, {i, 0, m}]` denotes a symbolic sum. The syntax is the same as for `Sum`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [OPESumExplicit](OPESumExplicit.md), [OPESumSimplify](OPESumSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


OPESum[SO[p]^OPEiSO[k]^(OPEm-OPEi-3),{OPEi,0,OPEm-3}]
OPESumExplicit[%]


OPESum[a^ib^(j-i)c^(m-j-4),{i,0,j},{j,0,m-4}]
OPESumExplicit[%]



