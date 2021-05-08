 
(* ::Section:: *)
(* Collect3 *)
(* ::Text:: *)
(*Collect3[expr, {x, y, ...}] collects terms involving the same powers of monomials $x^{n_1}$$y^{n_2}$ ... An option Factor -> True/False can be  given, which factors the coefficients. The option Head (default Plus) determines the applied function to the list of monomials  mulitplied by their coefficients..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*Collect2, Isolate.*)



(* ::Subsection:: *)
(* Examples *)



Collect3[2 a (b-a) (h-1)-b^2 (e a-c)+b^2,{a,b}]

Collect3[Expand[(a-b-c-d)^5],{a}]
