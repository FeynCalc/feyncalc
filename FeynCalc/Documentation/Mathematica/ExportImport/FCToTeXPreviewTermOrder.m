(* ::Package:: *)

 


(* ::Section:: *)
(*FCToTeXPreviewTermOrder*)


(* ::Text:: *)
(*`FCToTeXPreviewTermOrder[exp]` displays the output of `FCToTeXReorder` using the built-in Plus and Times but preserving the original ordering.*)


(* ::Text:: *)
(*Use `ReleaseHold` or `FRH` to allow Mathematica return to its original ordering.*)


(* ::Text:: *)
(*Notice that the output of `FCToTeXPreviewTermOrder` is not suitable for algebraic manipulations but should be understood as an intermediate expression form created to serve as an input for `TeXForm`*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCToTeXReorder](FCToTeXReorder.md).*)


(* ::Subsection:: *)
(*Examples*)


ex={(2*z*(11050 + 3438*L1 + 108*L2 + 75*NL + 150*NV + 12*Pi^2 + 432*Log[z]))/27, 
(-13629 - 4452*L1 + 24*L2 + 380*NH + 75*L1*NH + 130*NL + 150*L1*NL + 130*NV + 
150*L1*NV + 20*Sqrt[3]*Pi - 75*Sqrt[3]*NH*Pi + 360*Pi^2)/81, Plus}


FCToTeXPreviewTermOrder[ex]
