 
(* ::Section:: *)
(*B0Real*)
(* ::Text:: *)
(*`B0Real` is an option of `B0` (default `False`). If set to `True`, `B0` is assumed to be real and the relation `B0[a,0,a] = 2 + B0[0,a,a]` is applied.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [B0](B0.md).*)



(* ::Subsection:: *)
(*Examples*)
(* ::Text:: *)
(*By default the arguments are not assumed real.*)


B0[s,0,s]


(* ::Text:: *)
(*With B0Real\[Rule]True, transformation is done.*)


B0[s,0,s,B0Real->True,B0Unique->True]
