(* ::Package:: *)

 


(* ::Section:: *)
(*NumericQ1*)


(* ::Text:: *)
(*`NumericQ1[x, {a, b, ..}]` is like `NumericQ`, but assumes that `{a,b,..}` are numeric quantities.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


NumericQ[3 a+Log[b]+c^2]


NumericQ1[3 a+Log[b]+c^2,{}]


NumericQ1[3 a+Log[b]+c^2,{a,b,c}]
