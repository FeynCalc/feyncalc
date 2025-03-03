(* ::Package:: *)

 


(* ::Section:: *)
(*SUNSimplify*)


(* ::Text:: *)
(*`SUNSimplify[exp]` simplifies color algebraic expressions involving color matrices with implicit (`SUNT`) or explicit fundamental indices (`SUNTF`) as well as structure constants (`SUND`, `SUNF`) and Kronecker deltas (`SD`, `SDF`).*)


(* ::Text:: *)
(*If the option `Explicit` is set to `True` (default is `False`), the structure constants will be rewritten in terms of traces. However, since traces with 2 or 3 color matrices are by default converted back into structure constants, you must also set the option `SUNTraceEvaluate` to `False` (default is `Automatic`) in order to have unevaluated color traces in the output.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNTrace](SUNTrace.md), [SUNT](SUNT.md), [SUNTF](SUNTF.md), [SUNF](SUNF.md), [SUND](SUND.md), [SUNTraceEvaluate](SUNTraceEvaluate.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNDelta[a,b] SUNDelta[b,c]

SUNSimplify[%]


SUNT[a] . SUNT[a]

SUNSimplify[%]


SUNSimplify[SUNT[a] . SUNT[a],SUNNToCACF->False]


SUNF[a,r,s]SUNF[b,r,s]

SUNSimplify[%]


SUNF[a,b,c]  SUNF[a,b,c]

SUNSimplify[%]


SUNF[a,b,c] SUNF[d,b,c]

SUNSimplify[%]


SUNF[a,b,c] SUND[d,b,c]

SUNSimplify[%,Explicit->True]


SUND[a,b,c] SUND[a,b,c]

SUNSimplify[%,SUNNToCACF->False]//Factor2


SUNSimplify[SUND[a,b,c] SUND[e,b,c],SUNNToCACF->False]//Simplify


SUNSimplify[SUNF[a,b,c],Explicit->True]


SUNSimplify[SUNF[a,b,c],Explicit->True,SUNTraceEvaluate->False]


SUNSimplify[SUND[a,b,c],Explicit->True]


SUNSimplify[SUND[a,b,c],Explicit->True,SUNTraceEvaluate->False]


SUNF[a,b,c] SUNT[c,b,a]

SUNSimplify[%]


SUNF[a,b,e]SUNF[c,d,e]+SUNF[a,b,z]SUNF[c,d,z]

SUNSimplify[%,SUNIndexNames->{j}]


SUNSimplify[1-SD[i,i]]


SUNSimplify[SUNF[a,b,c] SUND[d,b,c]]


SUNSimplify[SUNF[a,b,c] SUND[a,b,d]]


SUNSimplify[SUNF[a,b,c] SUND[a,d,c]]


SUNSimplify[SUND[a,b,c] SUND[d,b,c]]


SUNSimplify[SUNTrace[SUNT[i1,i2,i1,i2]],FCE->True]


(* ::Text:: *)
(*`SUNSimplify` can also deal with chains of color matrices containing explicit fundamental indices (entered as `SUNTF`)*)


SUNTF[{a},i,j]SUNTF[{a},k,l]

SUNSimplify[%]


SUNTF[{b,a,c},i,j]SUNTF[{d,a,e},k,l]

SUNSimplify[%]


SUNTF[{a},i,j]SUNTrace[SUNT[b,a,c]]

SUNSimplify[%]



