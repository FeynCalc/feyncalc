(* ::Package:: *)

 


(* ::Section:: *)
(*SUNDeltaContract*)


(* ::Text:: *)
(*`SUNDeltaContract[exp]` substitutes for all `SUNDelta` in `exp` `SUNDeltaContract`, contracts the adjoint $\text{SU}(N)$ indices and resubstitutes `SUNDelta`.   `SUNDeltaContract[i, j]` is the Kronecker-delta for $\text{SU}(N)$ in the adjoint representation with contraction properties. It wraps the head SUNIndex around its arguments.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNDelta](SUNDelta.md), [SUNIndex](SUNIndex.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNDelta[SUNIndex[a],SUNIndex[b]]^2
SUNDeltaContract[%]
