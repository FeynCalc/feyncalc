(* ::Package:: *)

(* ::Section:: *)
(*SelectNotFree2*)


(* ::Text:: *)
(*`SelectNotFree2[expr, a, b, ...]` is like `SelectNotFree` but it first expands the expression w.r.t. the arguments via `Expand2`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FreeQ2](FreeQ2), [SelectFree](SelectFree), [SelectNotFree](SelectNotFree), [SelectFree2](SelectFree2).*)


(* ::Subsection:: *)
(*Examples*)


SelectNotFree[(a+b)c,b]


SelectNotFree2[(a+b)c,b]
