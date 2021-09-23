(* ::Package:: *)

 


(* ::Section:: *)
(*SUNF*)


(* ::Text:: *)
(*`SUNF[a, b, c]` are the structure constants of $SU(N)$. The arguments `a, b, c` should be of symbolic type.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUND](SUND.md), [SUNDelta](SUNDelta.md), [SUNIndex](SUNIndex.md), [SUNSimplify](SUNSimplify.md), [SUNT](SUNT.md), [Trick](Trick.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNF[a,b,c]x+SUNF[b,a,c]
Calc[%]
SUNSimplify[%%]


SUNF[a,a,b]
%//Calc


(* ::Text:: *)
(*This is a consequence of the usual choice for the normalization of the $T_a$ generators.*)


SUNF[a,b,c,Explicit->True]


SUNSimplify[SUNF[a,b,c] SUNF[a,b,d]]


SUNSimplify[SUNF[a,b,c],Explicit->True]


SUNF[a,b,c]//StandardForm


SUNF[a,b,c]//FCI//StandardForm


SUNF[a,b,c]//FCI//FCE//StandardForm


SUNF[b,a,c]
%//FCI
