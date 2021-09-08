(* ::Package:: *)

(* ::Section:: *)
(*SelectFree*)


(* ::Text:: *)
(*`SelectFree[expr, a, b, ...]` is equivalent to `Select[expr, FreeQ2[#, {a,b, ...}]&]`, except the special cases: `SelectFree[a, b]` returns `a` and `SelectFree[a,a]` returns 1 (where `a` is not a product or a sum).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md), [SelectNotFree](SelectNotFree.md).*)


(* ::Subsection:: *)
(*Examples*)


SelectFree[a+b+f[a]+d,a]


SelectFree[x y, x]


SelectFree[2 x y z f[x], {x,y}]


SelectFree[a,b]


SelectFree[a,a]


SelectFree[1,c]


SelectFree[f[x],x]
