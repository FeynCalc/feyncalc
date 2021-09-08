(* ::Package:: *)

 


(* ::Section:: *)
(*Series2*)


(* ::Text:: *)
(*`Series2` performs a series expansion around `0`. `Series2` is (up to the `Gamma`-bug in Mathematica versions smaller than 5.0) equivalent to `Series`, except that it applies `Normal` on the result and has an option `FinalSubstitutions`.*)


(* ::Text:: *)
(*`Series2[f, e, n]` is equivalent to `Series2[f, {e, 0, n}]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Series3](Series3.md).*)


(* ::Subsection:: *)
(*Examples*)


Series2[(x (1-x))^(\[Delta]/2),\[Delta],1]


Series2[Gamma[x],x,1]


Series[Gamma[x],{x,0,1}]


Series2[Gamma[x],x,2]


Series2[Gamma[x],x,2,FinalSubstitutions->{}]//FullSimplify


Series[Gamma[x],{x,0,If[$VersionNumber<5,4,2]}]//Normal//Expand//FullSimplify


(* ::Text:: *)
(*There is a table of expansions of special hypergeometric functions.*)


Series2[HypergeometricPFQ[{1,OPEm-1,Epsilon/2+OPEm},{OPEm,OPEm+Epsilon},1],Epsilon,1]


Series2[HypergeometricPFQ[{1, OPEm, Epsilon/2 + OPEm}, {1 + OPEm, Epsilon + OPEm},  1],Epsilon,1]


Hypergeometric2F1[1, Epsilon, 1 + 2 Epsilon,x]
Series2[%,Epsilon,3]


(* ::Text:: *)
(*There are over 100 more special expansions of ${}_2 F_1$ tabulated in `Series2.m`. The interested user can consult the source code (search for HYPERLIST).*)
