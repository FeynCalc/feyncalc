(* ::Package:: *)

 


(* ::Section:: *)
(*SUNSimplify*)


(* ::Text:: *)
(*`SUNSimplify[exp]` simplifies products of `SUNT` and `SUNTF` matrices in the expression.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNTrace](SUNTrace.md), [SUNT](SUNT.md), [SUNTF](SUNTF.md), [SUNF](SUNF.md), [SUND](SUND.md).*)


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


SUNSimplify[SUND[a,b,c],Explicit->True]


SUNF[a,b,c] SUNT[c,b,a]
SUNSimplify[%]


SUNF[a,b,e]SUNF[c,d,e]+SUNF[a,b,z]SUNF[c,d,z]
SUNSimplify[%,Explicit->False]
SUNSimplify[%,Explicit->False,SUNIndexRename->False]


SUNSimplify[1-SD[i,i]]


SUNSimplify[SUNF[a,b,c] SUND[d,b,c]]


SUNSimplify[SUNF[a,b,c] SUND[a,b,d]]


SUNSimplify[SUNF[a,b,c] SUND[a,d,c]]


SUNSimplify[SUND[a,b,c] SUND[d,b,c]]
