(* ::Package:: *)

 


(* ::Section:: *)
(*FCFactorOut*)


(* ::Text:: *)
(*`FCFactorOut[exp, pref]` factors out `pref` out of `exp`. This is often needed to bring `exp` into a particular form that Mathematica refuses to give.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md).*)


(* ::Subsection:: *)
(*Examples*)


FCFactorOut[(a+3 b),3 b]


FCFactorOut[(a+3 b),3 b,Head->hold]


(* ::Text:: *)
(*`FCFactorOut` is also an option of `Collect2`*)


x^2+6 y
Collect2[%,{x,y},FCFactorOut->3]



