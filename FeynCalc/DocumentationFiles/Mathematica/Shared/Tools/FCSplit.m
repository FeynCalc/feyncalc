 
(* ::Section:: *)
(* FCSplit *)
(* ::Text:: *)
(*FCSplit[exp, {v1, v2, ...}] splits expr into pieces that are free of any occurence of v1, v2, ... and pieces that contain those variables. This works both on sums and products. The output is provided in the form of a two element list. One can recover the original expression by applying Total to that list..*)


(* ::Subsection:: *)
(* Examples *)
FCSplit[(a+b)^2,{a}]

FCSplit[(a+b+c)^2,{a,b}]
