(* ::Package:: *)

 


(* ::Section:: *)
(*SUNTrace*)


(* ::Text:: *)
(*`SUNTrace[exp]` is the head of color traces. By default the trace is not evaluated. The evaluation occurs only when the option `SUNTraceEvaluate` is set to `True`. It is recommended to use `SUNSimplify`, which will automatically evaluate all color traces involving 2 or 3 matrices in the input expression.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNSimplify](SUNSimplify.md), [SUNFierz](SUNFierz.md), [SUNT](SUNT.md), [SUNTF](SUNTF.md), [SUNF](SUNF.md), [SUND](SUND.md), [SUNTraceEvaluate](SUNTraceEvaluate.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNTrace[SUNT[a,b]]


SUNTrace[SUNT[a,b],SUNTraceEvaluate->True]


SUNTrace[SUNT[a,b]]//SUNSimplify


SUNTrace[SUNT[a,b,c]]//SUNSimplify


SUNTrace[SUNT[a,b,c,d]]//SUNSimplify[#,SUNTraceEvaluate->True,SUNIndexNames->{j}]&


SUNTrace[SUNT[a,b,c,a,b,c]]//SUNSimplify
