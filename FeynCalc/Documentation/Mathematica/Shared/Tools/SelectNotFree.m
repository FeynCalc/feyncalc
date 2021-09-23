(* ::Package:: *)

 


(* ::Section:: *)
(*SelectNotFree*)


(* ::Text:: *)
(*`SelectNotFree[expr, x]` returns that part of `expr` which is not free of any occurrence of `x`.*)


(* ::Text:: *)
(*`SelectNotFree[expr, a, b, ...]` is equivalent to `Select[expr, !FreeQ2[#, {a, b, ...}]&]`, except the special cases:*)
(*`SelectNotFree[a, b]` returns `1` and `SelectNotFree[a, a]` returns `a` (where `a` is not a product or a sum).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md), [SelectFree](SelectFree.md).*)


(* ::Subsection:: *)
(*Examples*)


SelectNotFree[a+b+f[a],a]


SelectNotFree[2 x y f[x] z,{x,y}]


SelectNotFree[a,b]


SelectNotFree[a+x,b]


SelectNotFree[a,a]


SelectNotFree[1,c]


SelectNotFree[f[x],x]
