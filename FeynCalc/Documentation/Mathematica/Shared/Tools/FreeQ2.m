(* ::Package:: *)

 


(* ::Section:: *)
(*FreeQ2*)


(* ::Text:: *)
(*`FreeQ2[expr, {form1, form2, ...}]` yields `True` if `expr` does not contain any occurrence of `form1, form2, ...` and `False` otherwise.*)


(* ::Text:: *)
(*`FreeQ2[expr, form]` is the same as `FreeQ[expr, form]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SelectFree](SelectFree.md), [SelectNotFree](SelectNotFree.md).*)


(* ::Subsection:: *)
(*Examples*)


FreeQ2[x+f[x]+y, {a,x}]


FreeQ2[x+f[x]+y,{a,b}]


FreeQ2[x, y]


FreeQ2[f[x], f]
