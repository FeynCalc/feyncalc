(* ::Package:: *)

 


(* ::Section:: *)
(*SelectFree2*)


(* ::Text:: *)
(*`SelectFree2[expr, a, b, ...]` is similar to `SelectFree` but it also differs from the latter in several respects.*)


(* ::Text:: *)
(*If `expr` is  a list, `SelectFree2` behaves exactly the same way as `SelectFree`.*)


(* ::Text:: *)
(*If `expr` is not a list, `SelectFree2` first expands the expression w.r.t. the arguments via `Expand2`.*)


(* ::Text:: *)
(*Furthermore, `SelectFree2[a,b]` returns `a` and `SelectFree2[a,a]` returns `0`. This differs from the behavior of `SelectFree` but is consistent with the naive expectations when applying the function to a sum of terms.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md), [SelectFree](SelectFree.md), [SelectNotFree](SelectNotFree.md), [SelectNotFree2](SelectNotFree2.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Note the difference between SelectFree and SelectFree2*)


SelectFree[(a+b)c,b]


SelectFree2[(a+b)c,b]


SelectFree[a,b]


SelectFree2[a,b]


SelectFree[a,a]


SelectFree2[a,a]


(* ::Text:: *)
(*When there are hidden zeros, `SelectFree2` obviously works better*)


SelectFree[(a-b+c)^2-(a^2-2 a b+2 a c+b^2-2 b c+c^2),a]


SelectFree2[(a-b+c)^2-(a^2-2 a b+2 a c+b^2-2 b c+c^2),a]
