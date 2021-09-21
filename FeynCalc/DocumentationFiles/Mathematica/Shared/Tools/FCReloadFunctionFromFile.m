(* ::Package:: *)

 


(* ::Section:: *)
(*FCReloadFunctionFromFile*)


(* ::Text:: *)
(*`FCReloadFunctionFromFile[function, path]` is an auxiliary function that attempts to remove all the definitions of the given FeynCalc function and then reload them from the specified file.*)


(* ::Text:: *)
(*It is intended to be a helper tool for FeynCalc developers, which allows one to debug/improve internal functions and test the results without restarting the kernel. Depending on the complexity of the given function, there might also be unknown side effects.*)


(* ::Text:: *)
(*The function is not meant to be invoked by the normal users.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCReloadAddOns](FCReloadAddOns.md).*)


(* ::Subsection:: *)
(*Examples*)
