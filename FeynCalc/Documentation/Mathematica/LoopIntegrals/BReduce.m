 
(* ::Section:: *)
(*BReduce*)
(* ::Text:: *)
(*`BReduce` is an option for `B0`, `B00`, `B1`, `B11` determining whether reductions to `A0` and `B0` will be done.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [A0](A0.md), [B0](B0.md), [B00](B00.md), [B1](B1.md), [B11](B11.md).*)



(* ::Subsection:: *)
(*Examples*)
(* ::Text:: *)
(*By default $B_0$ is not expressed in terms of $A_0$.*)


B0[0,s,s]


(* ::Text:: *)
(*With BReduce\[Rule]True, transformation is done.*)


B0[0,s,s,BReduce->True]
