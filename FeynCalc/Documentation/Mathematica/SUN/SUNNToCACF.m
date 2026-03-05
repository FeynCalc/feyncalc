(* ::Package:: *)

 


(* ::Section:: *)
(*SUNNToCACF*)


(* ::Text:: *)
(*`SUNNToCACF` is an option of `SUNSimplify` and `CalcColorFactor`. If set to `True`, the Casimir operator eigenvalues `CA` ($=n_c$) and `CF` ($=(n_c^2-1)/(2 n_c)$) are reconstructed from the result in terms of $n_c$ using heuristics. The reconstruction is not always perfect, but mostly sufficient at tree and one-loop level.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SUNSimplify](SUNSimplify.md), [SUNN](SUNN.md), [CA](CA.md), [CF](CF.md).*)


(* ::Subsection:: *)
(*Examples*)


SUNSimplify[SUNDelta[SUNIndex[a], SUNIndex[a]]]


SUNSimplify[SUNDelta[SUNIndex[a], SUNIndex[a]],SUNNToCACF->False]
