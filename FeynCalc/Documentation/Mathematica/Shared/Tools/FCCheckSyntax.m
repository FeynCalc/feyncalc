(* ::Package:: *)

 


(* ::Section:: *)
(*FCCheckSyntax*)


(* ::Text:: *)
(*`FCCheckSyntax[exp]` attempts to detect mistakes and inconsistencies in the user input. The function returns the original expression but will abort the evaluation if it thinks that the input is incorrect. Notice that false positives are possible and it is not guaranteed that the input which passes `FCCheckSyntax` is indeed fully correct.*)


(* ::Text:: *)
(*`FCCheckSyntax` is also an option for several FeynCalc routines. If set to `True`, those functions will try to check the syntax of the input expressions to detect possible inconsistencies. However, on large expressions such checks may cost a lot of performance, which is why this option is set to `False` by default.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Typical mistake, using `Times` instead of `Dot` in noncommutative products*)


FCCheckSyntax[GA[mu]*GA[nu]]


(* ::Text:: *)
(*Another common mistake, Einstein summation convention is violated*)


FCCheckSyntax[FV[p,\[Mu]]FV[q,\[Mu]] FV[r,\[Mu]]]



