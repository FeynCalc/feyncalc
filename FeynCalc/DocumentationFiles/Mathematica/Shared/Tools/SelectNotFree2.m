(* ::Package:: *)

 


(* ::Section:: *)
(*SelectNotFree2*)


(* ::Text:: *)
(*`SelectNotFree2[expr, a, b, ...]` is similar to `SelectNotFree` but it also differs from the latter in several respects.*)


(* ::Text:: *)
(*If `expr` is  a list, `SelectNotFree2` behaves exactly the same way as `SelectNotFree`.*)


(* ::Text:: *)
(*If `expr` is not a list, `SelectNotFree2` first expands the expression w.r.t. the arguments via `Expand2`.*)


(* ::Text:: *)
(*Furthermore, `SelectNotFree2[a,b]` returns `0`. This differs from the behavior of `SelectFree` but is consistent with the naive expectations when applying the function to a sum of terms.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FreeQ2](FreeQ2.md), [SelectFree](SelectFree.md), [SelectNotFree](SelectNotFree.md), [SelectFree2](SelectFree2.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Note the difference between SelectNotFree and SelectNotFree2*)


SelectNotFree[(a+b)c,b]


SelectNotFree2[(a+b)c,b]


SelectNotFree[a,b]


SelectNotFree2[a,b]


(* ::Text:: *)
(*Here the behavior is identical*)


SelectNotFree[a,a]


SelectNotFree2[a,a]


(* ::Text:: *)
(*When there are hidden zeros, `SelectNotFree2` obviously works better*)


SelectNotFree[(a-b+c)^2-(a^2-2 a b+2 a c+b^2-2 b c+c^2),a]


SelectNotFree2[(a-b+c)^2-(a^2-2 a b+2 a c+b^2-2 b c+c^2),a]
