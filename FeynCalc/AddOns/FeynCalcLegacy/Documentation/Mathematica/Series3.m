(* ::Package:: *)

 


(* ::Section:: *)
(*Series3*)


(* ::Text:: *)
(*`Series3` performs a series expansion around `0`. `Series3` is equivalent to `Series`, except that it applies `Normal` on the result and that some `Series` bugs are fixed.*)


(* ::Text:: *)
(*`Series3[f, e, n]` is equivalent to  `Series3[f, {e, 0, n}]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Series2](Series2.md).*)


(* ::Subsection:: *)
(*Examples*)


Series3[(x (1-x))^(\[Delta]/2),\[Delta],1]


Series3[Gamma[x],x,1]//FullSimplify
