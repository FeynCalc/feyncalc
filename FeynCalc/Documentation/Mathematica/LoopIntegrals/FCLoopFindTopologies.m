(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopFindTopologies*)


(* ::Text:: *)
(*`FCLoopFindTopologies[exp, {q1, q2, ...}]` attempts to identify the loop integral topologies present in `exp` by looking at the propagator denominators that depend on the loop momenta `q1, q2, ...` . It returns a list of two entries, where the first one is the original expression with the denominators rewritten as `GLI`s, and the second one is the set of the identified topologies.*)


(* ::Text:: *)
(*Each of the identified topologies must contain linearly independent propagators (unless the option `FCLoopBasisOverdeterminedQ` is set to True), but may lack propagators needed to form a complete basis.*)


(* ::Text:: *)
(*Scaleless topologies are automatically removed, but this can be disabled by setting the option `FCLoopScalelessQ` to `True`.*)


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


(* ::Text:: *)
(*Find topologies occurring in the 2-loop QCD corrections to the $B_s$-meson mixing*)


topos=Get[FileNameJoin[{$FeynCalcDirectory,"Documentation","Examples",
"Topologies","BMixingTopos2L.m"}]];


topos//Length


res=FCLoopFindTopologies[topos,{k1,k2}];


(* ::Text:: *)
(*Show the first two topologies*)


(res//Last)[[1;;2]]


(* ::Text:: *)
(*In practical calculations even the simple extraction of topologies from the given list of diagrams can take considerable amount of time. This is why it is better to parallelize this process as much as possible.*)


(* ::Text:: *)
(*We can split this part into two steps, were we first apply `FCLoopIsolate` to the resulting amplitudes and then pass the output to `FCLoopFindTopologies`. To avoid the unnecessary application of `FCLoopIsolate` during this step, we should specify the head with which the topologies have been wrapped via the option `FCLoopIsolate`.*)


isolatedTopos=FCLoopIsolate[topos[[44;;48]],{k1,k2},  Collecting-> False, Factoring -> False, Numerator -> False, Head -> loopDen];


res=FCLoopFindTopologies[isolatedTopos,{k1,k2},FCLoopIsolate->loopDen,Head->ampDen,Collecting->False];
