(* ::Package:: *)

(* ::Section:: *)
(*FCLoopToGLI*)


(* ::Text:: *)
(*`FCLoopToGLI[int, lmoms]` converts the integral `int` depending on the loop momenta `lmoms` to the GLI-notation. The function returns a `GLI`-integral and a minimal `FCTopology` containing only the propagators from the original integral.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopFromGLI](FCLoopToGLI.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopValidTopologyQ](FCLoopValidTopologyQ.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopToGLI[FAD[{k,m}],{k}]


FCLoopToGLI[FAD[{k1,m1},{k2,m2},{k1-k2}],{k1,k2}]
