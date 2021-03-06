 
(* ::Section:: *)
(* FreeQ2 *)
(* ::Text:: *)
(*FreeQ2[expr, {form1, form2, ...}] yields True if expr does not contain any occurence of form1, form2, ... and False otherwise. FreeQ2[expr, form] is the same as FreeQ[expr, form]..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*SelectFree, SelectNotFree.*)



(* ::Subsection:: *)
(* Examples *)



FreeQ2[x+f[x]+y, {a,x}]

FreeQ2[x+f[x]+y,{a,b}]

FreeQ2[x, y]

FreeQ2[f[x], f]
