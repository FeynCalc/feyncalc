(* ::Package:: *)

 


(* ::Section:: *)
(*SUNN*)


(* ::Text:: *)
(*`SUNN` denotes the number of colors. `Trick[SUNDelta[a, a]]` yields $n_c^2 -1$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNSimplify](SUNSimplify.md), [Trick](Trick.md), [SUNIndex](SUNIndex.md), [CA](CA.md), [CF](CF.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNSimplify[SUNDelta[SUNIndex[a], SUNIndex[a]],SUNNToCACF->False]
