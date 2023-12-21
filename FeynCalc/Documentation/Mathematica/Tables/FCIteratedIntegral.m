(* ::Package:: *)

 


(* ::Section:: *)
(*FCIteratedIntegral*)


(* ::Text:: *)
(*`FCIteratedIntegral[f,x,a,b]` is a special head indicating that the function $f$ represents an iterated integral or a linear combination thereof and that it should be integrated in $x$ from $a$ to $b$. This notation is understood by the function `FCIteratedIntegralEvaluate` that does the actual integration.*)


(* ::Text:: *)
(*Notice that before applying `FCIteratedIntegralEvaluate` all rational functions of $x$ in $f$ should be converted to the `FCPartialFractionForm`representation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCIteratedIntegralEvaluate](FCIteratedIntegralEvaluate.md), [ToFCPartialFractionForm](ToFCPartialFractionForm.md)*)


(* ::Subsection:: *)
(*Examples*)


fun=1/(1+x)


int=FCIteratedIntegral[ToFCPartialFractionForm[fun,x],x,a,b]


FCIteratedIntegralEvaluate[int]
