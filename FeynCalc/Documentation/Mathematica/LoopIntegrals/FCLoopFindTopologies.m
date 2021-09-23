(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFindTopologies*)


(* ::Text:: *)
(*`FCLoopFindTopologies[exp, {q1, q2, ...}]` attempts to identify the loop integral topologies present in `exp` by looking at the propagator denominators that depend on the loop momenta `q1, q2, ...` . It returns a list of two entries, where the first one is the original expression with the denominators rewritten as `GLI`s, and the second one is the set of the identified topologies. Each of the identified topologies must contain linearly independent propagators (unless the option `FCLoopBasisOverdeterminedQ` is set to True), but may lack propagators needed to form a complete basis.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Find topologies occurring in the 2-loop ghost self-energy amplitude*)


amp=Get[FileNameJoin[{$FeynCalcDirectory,"Documentation","Examples",
"Amplitudes","Gh-Gh-2L.m"}]];


res=FCLoopFindTopologies[amp,{q1,q2}];


res//Last
