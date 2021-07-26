(* ::Package:: *)

 


(* ::Section:: *)
(*SUNNToCACF*)


(* ::Text:: *)
(*`SUNNToCACF` is an option of `SUNSimplify` and `CalcColorFactor`. If set to `True`, the Casimir operator eigenvalues `CA` ($=n_c$) and `CF` ($=(n_c^2-1)/(2 n_c)$) are introduced.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[CalcColorFactor](CalcColorFactor), [SUNSimplify](SUNSimplify), [Trick](Trick), [SUNN](SUNN), [CA](CA), [CF](CF).*)


(* ::Subsection:: *)
(*Examples*)


SUNSimplify[SUNDelta[SUNIndex[a], SUNIndex[a]],SUNNToCACF->True]
