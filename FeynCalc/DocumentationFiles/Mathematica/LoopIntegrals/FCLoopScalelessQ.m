(* ::Package:: *)

(* ::Section:: *)
(*FCLoopScalelessQ*)


(* ::Text:: *)
(*`FCLoopScalelessQ[int, {p1, p2, ...}]` checks whether the loop integral `int` depending on the loop momenta `p1, p2, ...` is scaleless. Only integrals that admit a Feynman parametrization with proper $U$ and $F$ polynomials are supported.*)


(* ::Text:: *)
(*The function uses the of Alexey Pak [arXiv:1111.0868](https://arxiv.org/abs/1111.0868). Cf. also the PhD thesis of Jens Hoff [10.5445/IR/1000047447](https://doi.org/10.5445/IR/1000047447) for the detailed description of a possible implementation.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCTopology](FCTopology), [GLI](GLI), [FCLoopToPakForm](FCLoopToPakForm), [FCLoopPakOrder](FCLoopPakOrder), [FCLoopScalelessQ](FCLoopScalelessQ).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A scaleless 2-loop tadpole*)


FCLoopScalelessQ[FAD[p1,p2,p1-p2],{p1,p2}]


(* ::Text:: *)
(*A 1-loop massless eikonal integral.*)


FCLoopScalelessQ[SFAD[{{0,2 p . q},0},p],{p}]


(* ::Text:: *)
(*A 1-loop massive eikonal integral.*)


FCLoopScalelessQ[SFAD[{{0,2 p . q},0},p],{p}]
