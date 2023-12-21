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


exp=((L2*(-5 + nc)*(1 + nc)*(-32*nc - 32*nc^2))/nc^3 + (L1*(1 + nc)*(672*nc + 256*nc^2 + 
32*nc^3 - 40*nc^2*NH - 80*nc^2*NL - 80*nc^2*NV))/(3*nc^3) + ((1 + nc)*(14544*nc + 
7872*nc^2 - 1440*nc^3 - 1216*nc^2*NH - 416*nc^2*NL - 416*nc^2*NV - 192*Sqrt[3]*nc*Pi + 
240*Sqrt[3]*nc^2*NH*Pi - 384*nc^3*Pi^2 - 1440*nc^2*NV*z))/(36*nc^3) + 
  ((1 + nc)*(14544*nc + 7872*nc^2 - 1440*nc^3 - 1216*nc^2*NH - 416*nc^2*NL - 
  416*nc^2*NV - 192*Sqrt[3]*nc*Pi + 240*Sqrt[3]*nc^2*NH*Pi - 384*nc^3*Pi^2 + 
  11520*nc*z + 15984*nc^2*z + 3312*nc^3*z - 1440*nc^2*NL*z - 2880*nc^2*NV*z - 
  768*nc^3*Pi^2*z))/(36*nc^3))/2


(* ::Text:: *)
(*Split into pieces that depend on `L1`, `L2` and those then don' t . Then collect terms in the first group w.r.t `L1, L2` . Collect terms in the second group w.r.t. `z` . Use `ExpandAll` as the factoring function in both groups . Sort the resulting terms in the first group such, that terms containing `L1` come first, then those with `L2` and finally all the rest . Put terms that depend on `z` in the second group first .*)


out1=FCToTeXReorder[exp,{{L1,L2},{L1,L2},{z}},Split->True,Factoring->{Function[x,
ExpandAll[x]],Function[x,ExpandAll[x]]},SortBy->{Function[x,Which[!FreeQ2[x,{L1}],1,
!FreeQ2[x,{L2}],2,True,30]],Function[x,Which[!FreeQ2[x,{z}],1,True,3]]}]


(* ::Text:: *)
(*Now work with the innermost brackets and put terms that contain `z` first . All the other terms should be sorted, such that `NH`, `NV` and `NL` terms appear in this order.*)


out2=FCToTeXReorder[out1,{{},{},{}},Split->False,Factoring->{Function[x,ExpandAll[x]],
Function[x,ExpandAll[x]]},SortBy->{Function[x,Which[!FreeQ2[x,{z}],1,!FreeQ2[x,{NH}],
2,!FreeQ2[x,{NV}],3,!FreeQ2[x,{NL}],4,True,5]],Function[x,Which[!FreeQ2[x,{z}],
1,!FreeQ2[x,{NH}],2,!FreeQ2[x,{NV}],3,!FreeQ2[x,{NL}],4,True,5]]}]


FCToTeXPreviewTermOrder[out2]
