(* ::Package:: *)

 


(* ::Section:: *)
(*SUNT*)


(* ::Text:: *)
(*`SUNT[a]` is the $SU(N)$ $T^a$ generator in the fundamental representation. The fundamental indices are implicit.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [CA](CA.md), [CF](CF.md), [SUND](SUND.md), [SUNDelta](SUNDelta.md), [SUNF](SUNF.md), [SUNSimplify](SUNSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNT[a]


(* ::Text:: *)
(*Since $T^a$ is a noncommutative object, products have to separated by a `Dot` (`.`).*)


SUNT[a] . SUNT[b] . SUNT[c]


SUNT[a,b,c,d]


SUNSimplify[SUNT[a,b,a],SUNNToCACF->False]


SUNSimplify[SUNT[a,b,b,a]]


SUNSimplify[SUNT[a,b,a]]


SUNSimplify[SUNT[a,b,a],SUNNToCACF->False]


(* ::Text:: *)
(*The normalization of the generators is chosen in the standard way, therefore $\textrm{Tr}(T^aT^b) = \frac{1}{2} \delta _{ab}$*)


SUNTrace[SUNT[a,b]]


(* ::Text:: *)
(*In case you want $T_f$, you need to include a factor `2*Tf`inside the trace.*)


SUNTrace[2 Tf SUNT[a,b]]


SUNTrace[SUNT[a,b]]//StandardForm


SUNT[a]//FCI//StandardForm


SUNT[a]//FCI//FCE//StandardForm
