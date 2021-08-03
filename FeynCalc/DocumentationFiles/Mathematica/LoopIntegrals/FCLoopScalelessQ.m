(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopScalelessQ*)


(* ::Text:: *)
(*`FCLoopScalelessQ[int, {p1, p2, ...}]` checks whether the loop integral `int` depending on the loop momenta `p1, p2, ...` is scaleless. Only integrals that admit a Feynman parametrization with proper $U$ and $F$ polynomials are supported.*)


(* ::Text:: *)
(*Cf. arXiv:1011.4863 and the PhD thesis of Jens Hoff (10.5445/IR/1000047447) for the description of the underlying algorithm.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCTopology](FCTopology), [GLI](GLI), [FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakOrder](FCLoopPakOrder).*)


(* ::Subsection:: *)
(*Examples*)


FCClearScalarProducts[];
SPD[Q]=0;
FCLoopScalelessQ[FAD[p1,p2,Q-p1-p2,Q-p1,Q-p2],{p1,p2}]


FCClearScalarProducts[];
FCLoopScalelessQ[FAD[{k2,mg},{k3,mc},{k1-q},{k2-q,mb},{k1-k2},{k2-k3,mc}],{k1,k2,k3}]
