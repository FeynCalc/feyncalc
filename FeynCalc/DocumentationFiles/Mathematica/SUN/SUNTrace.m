(* ::Package:: *)

 


(* ::Section:: *)
(*SUNTrace*)


(* ::Text:: *)
(*`SUNTrace[exp]` calculates the color-trace.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[SUNTrace](SUNTrace), [SUNT](SUNT), [SUNTF](SUNTF), [SUNF](SUNF), [SUND](SUND).*)


(* ::Subsection:: *)
(*Examples*)


SUNT[a,b]
SUNTrace[%]


SUNTrace[SUNT[a,b,c]]


SUNTrace[SUNT[a,b,c],Explicit->True]


SUNTrace[SUNT[a,b,c,d]]


SUNTrace[SUNT[a,b,c,d],Explicit->True]
SUNSimplify[%,Explicit->True]


SUNTrace[SUNT[a,b,c,d,e],Explicit->True]
SUNSimplify[%,Explicit->True]
