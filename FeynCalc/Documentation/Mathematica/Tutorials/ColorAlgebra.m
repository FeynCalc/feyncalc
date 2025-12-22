(* ::Package:: *)

 


(* ::Section:: *)
(*Color algebra*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md).*)


(* ::Subsection:: *)
(*Notation for colored objects*)


(* ::Text:: *)
(*FeynCalc objects relevant for the color algebra are*)


SUNT[a]


SUNF[a,b,c]


SUND[a,b,c]


SUNDelta[a,b]


SUNN


CA


CF


(* ::Text:: *)
(*There are two main functions to deal with colored objects: `SUNSimplify` and `SUNTrace`. In general, `SUNSimplify` will also simplify color traces when possible*)


SUNT[a,a]
SUNSimplify[%]


SUNT[a,b,a,b]
SUNSimplify[%]


SUNT[b,d,a,b,d]
SUNSimplify[%]


SUNF[a, r, s] SUNF[b, r, s]
SUNSimplify[%]


SUNF[a, b, c]  SUNF[a, b, c]
SUNSimplify[%]


SUNF[a, b, c] SUND[d, b, c] 
SUNSimplify[%]


SUND[a, b, c] SUND[a, b, c] 
SUNSimplify[%]


(* ::Text:: *)
(*The color factors $C_A$ and $C_F$ are reconstructed from $N_c$ using heuristics. The reconstruction can be disabled by setting the option `SUNNToCACF` to `False`*)


SUNSimplify[SUNT[b,d,a,b,d],SUNNToCACF->False]


(* ::Text:: *)
(*The color traces are not evaluated by default. The evaluation can be forced either by applying `SUNSimplify` or setting the option `SUNTraceEvaluate` to `True`*)


SUNTrace[SUNT[a,b]]


SUNTrace[SUNT[a,b,b,a]]


SUNTrace[SUNT[a,b]]//SUNSimplify


SUNTrace[SUNT[a,b,b,a]]//SUNSimplify


SUNTrace[SUNT[a,b],SUNTraceEvaluate->True]


(* ::Text:: *)
(*Use `SUNTF` to get color matrices with explicit fundamental indices*)


SUNTF[{a,b,c},i,j]SUNTrace[SUNT[b,a]]
%//SUNSimplify


SUNDelta[a, b] SUNTF[{a, b}, i, j] SUNTF[{c, d}, j, i]
SUNSimplify[%]


(* ::Text:: *)
(*Color traces with more than 3 distinct matrices are not evaluated by default (assuming that no other simplifications are possible). The evaluation can be forced using the option `SUNTraceEvaluate` set to `True`*)


SUNTrace[SUNT[a,b,c,d]]//SUNSimplify


SUNTrace[SUNT[a,b,c,d]]//SUNSimplify[#,SUNTraceEvaluate->True]&


(* ::Text:: *)
(*One can automatically rename dummy indices using the `SUNIndexNames` option*)


SUNTrace[SUNT[a,b,c,d]]//SUNSimplify[#,SUNTraceEvaluate->True,SUNIndexNames->{j}]&
