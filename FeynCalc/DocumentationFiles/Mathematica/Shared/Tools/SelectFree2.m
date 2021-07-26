(* ::Package:: *)

(* ::Section:: *)
(*SelectFree2*)


(* ::Text:: *)
(*`SelectFree2[expr, a, b, ...]` is like `SelectFree` but it first expands the expression w.r.t. the arguments via `Expand2`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FreeQ2](FreeQ2), [SelectFree](SelectFree), [SelectNotFree](SelectNotFree), [SelectNotFree2](SelectNotFree2).*)


(* ::Subsection:: *)
(*Examples*)


SelectFree[(a+b)c,b]


SelectFree2[(a+b)c,b]
