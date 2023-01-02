(* ::Package:: *)

 


(* ::Section:: *)
(*SUNTF*)


(* ::Text:: *)
(*`SUNTF[{a}, i, j]` is the $SU(N)$ $T^a$ generator in the fundamental representation. The fundamental indices are explicit.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNFindex](SUNFindex.md), [SUNT](SUNT.md), [SUNSimplify](SUNSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNTF[a,i,j]


SUNTF[{a,b},i,j]


(* ::Text:: *)
(*`SUNTF` are $c$-numbers, hence they are commutative objects and do not require a dot*)


SUNTF[{a,b},i,j]SUNTF[{c,d},j,k]


SUNTF[{a,b},i,j]SUNTF[{c,d},j,k]//SUNSimplify


(* ::Text:: *)
(*A chain with closed indices is automatically converted into a trace*)


SUNTF[{a,b},i,j]SUNTF[{c,d},j,i]//SUNSimplify


SUNDelta[a,b]SUNTF[{a,b},i,j]SUNTF[{c,d},j,i]//SUNSimplify


SUNTF[{a,b},i,j]//FCI//StandardForm
