(* ::Package:: *)

 


(* ::Section:: *)
(*FCToTeXReorder*)


(* ::Text:: *)
(*`FCToTeXReorder[exp, {{v1, v2, ... }, {a1, a2, ... }, {b1, b2, ... }}]` is an auxiliary function that helps to bring the given Mathematica expression `exp` into a form suitable for being inserted into a LaTeX document.*)


(* ::Text:: *)
(*To override the built-in ordering of `Plus` and `Times`, the expression is converted into a nested list made of elements of the form `{a, b, ... , Plus}` or `{a, b, ... ,Times}` for a sum or a product respectively.*)


(* ::Text:: *)
(*Then, the option `SortBy` allows to specify two sorting functions that will be used to reorder the terms in both groups.*)


(* ::Text:: *)
(*Most importantly, `FCToTeXReorder` can be  applied to the output of a previous function call. This allows for arbitrarily deep nesting.*)


(* ::Text:: *)
(*Finally, you can check if the final result satisfies your expectations by using `FCToTeXPreviewTermOrder`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCToTeXPreviewTermOrder](FCToTeXPreviewTermOrder.md).*)


(* ::Subsection:: *)
(*Examples*)


exp=(-13629 - 4452*L1 + 24*L2 + 380*NH + 75*L1*NH + 130*NL + 150*L1*NL + 
130*NV + 150*L1*NV + 20*Sqrt[3]*Pi - 75*Sqrt[3]*NH*Pi + 360*Pi^2 + 66300*z + 
20628*L1*z + 648*L2*z + 450*NL*z + 900*NV*z + 72*Pi^2*z + 2592*z*Log[z])/81;


aux1=FCToTeXReorder[exp,{{z},{Log,L1,L2},{Log,L1,L2}}]


aux1//FCToTeXPreviewTermOrder


aux1//InputForm


res=FCToTeXReorder[aux1,{{L1,L2},{NH,NV,NL},{NH,NV,NL}}]


res//FCToTeXPreviewTermOrder
