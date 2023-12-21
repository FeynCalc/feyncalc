(* ::Package:: *)

 


(* ::Section:: *)
(*MemSet*)


(* ::Text:: *)
(*`MemSet[f[x_], body]` is like `f[x_] := f[x] = body`, but depending on the value of the setting of `FCMemoryAvailable -> memorycut` (`memorycut` - `MemoryInUse[]/10^6`)*)


(* ::Text:: *)
(*`MemSet[f[x_], body]` may evaluate as `f[x_] := body`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCMemoryAvailable](FCMemoryAvailable.md).*)


(* ::Subsection:: *)
(*Examples*)
