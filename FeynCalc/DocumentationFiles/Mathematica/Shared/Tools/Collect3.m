(* ::Package:: *)

 


(* ::Section:: *)
(*Collect3*)


(* ::Text:: *)
(*`Collect3[expr, {x, y, ...}]` collects terms involving the same powers of monomials $x^{n_1}$, $y^{n_2}$, ...*)


(* ::Text:: *)
(*The option `Factor` can bet set to `True` or `False`, which factors the coefficients.*)


(* ::Text:: *)
(*The option `Head` (default `Plus`) determines the applied function to the list of monomials  multiplied by their coefficients.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md), [Isolate](Isolate.md).*)


(* ::Subsection:: *)
(*Examples*)


Collect3[2 a (b-a) (h-1)-b^2 (e a-c)+b^2,{a,b}]


Collect3[Expand[(a-b-c-d)^5],{a}]
