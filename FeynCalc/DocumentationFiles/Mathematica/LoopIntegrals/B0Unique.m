 
(* ::Section:: *)
(*B0Unique*)
(* ::Text:: *)
(*`B0Unique` is an option of `B0`. If set to `True`, `B0[0,0,m2]` is replaced with `(B0[0,m2,m2]+1)` and `B0[m2,0,m2]` simplifies to `(B0[0,m2,m2]+2)`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [B0](B0.md).*)



(* ::Subsection:: *)
(*Examples*)
(* ::Text:: *)
(*By default no transformation is done.*)


B0[0,0,s]


(* ::Text:: *)
(*With `B0Real->True` following transformation is applied*)


B0[0,0,s,B0Unique->True]
