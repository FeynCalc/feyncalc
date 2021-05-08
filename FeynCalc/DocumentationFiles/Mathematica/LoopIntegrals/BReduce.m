 
(* ::Section:: *)
(* BReduce *)
(* ::Text:: *)
(*BReduce is an option for B0, B00, B1, B11 determining whether reductions to A0 and B0 will be done..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*A0, B0, B00, B1, B11.*)



(* ::Subsection:: *)
(* Examples *)
(* ::Text:: *)
(*By default $B_0$ is not expressed in terms of $A_0$.*)


B0[0,s,s]


(* ::Text:: *)
(*With BReduce\[Rule]True, transformation is done.*)


B0[0,s,s,BReduce->True]
