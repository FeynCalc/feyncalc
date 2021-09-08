(* ::Package:: *)

 


(* ::Section:: *)
(*SUNFDeltaContract*)


(* ::Text:: *)
(*`SUNFDeltaContract[exp]` substitutes for all `SUNFDelta` in exp `SUNFDeltaContract`, contracts the fundamental $\text{SU}(N)$ indices and resubstitutes `SUNFDelta`.`SUNFDeltaContract[i, j]` is the Kronecker-delta for $\text{SU}(N)$ in the fundamental representation with contraction properties. It wraps the head `SUNFIndex` around its arguments.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNFDelta](SUNFDelta.md), [SUNFIndex](SUNFIndex.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNFDelta[SUNFIndex[a],SUNFIndex[b]]^2
SUNFDeltaContract[%]
