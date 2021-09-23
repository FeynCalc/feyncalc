(* ::Package:: *)

 


(* ::Section:: *)
(*Coefficient2*)


(* ::Text:: *)
(*`Coefficient2[exp, form1, form2, ...]` is like Coefficient, but it also allows to extracts coefficients  of `form1, form2, ...` sequentially. To specify the power in `formi`, write it as `{var,pow}`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Cases2](Cases2.md).*)


(* ::Subsection:: *)
(*Examples*)


ex=y0+ep y1+a4(1/ep x1 + x2 +x3 ep)+ a4^2(1/ep^2z1 +1/ep z2 + z3 +x4 ep)


Coefficient2[ex,a4]


Coefficient2[ex,a4,2]


Coefficient2[ex,{a4,2}]


Coefficient2[ex,{a4,2},{ep,-1}]


Coefficient2[ex,{a4,1},{ep,0}]


Coefficient2[ex,a4,ep]
