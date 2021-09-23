 
(* ::Section:: *)
(*FCMultiLoopTID*)
(* ::Text:: *)
(*`FCMultiLoopTID[amp, {q1, q2, ...}]` does a multi-loop tensor integral decomposition, transforming the Lorentz indices away from the loop momenta `q1, q2, ...` The decomposition is applied only to the loop integrals where loop momenta are contracted with Dirac matrices or epsilon tensors.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [TID](TID.md).*)


(* ::Subsection:: *)
(*Examples*)


FCI[FVD[q1,\[Mu]] FVD[q2,\[Nu]] FAD[q1,q2,{q1-p1},{q2-p1},{q1-q2}]]
FCMultiLoopTID[%,{q1,q2}]
