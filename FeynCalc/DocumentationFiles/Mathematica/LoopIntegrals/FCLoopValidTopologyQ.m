(* ::Package:: *)

(* ::Section:: *)
(*FCLoopValidTopologyQ*)


(* ::Text:: *)
(*`FCLoopValidTopologyQ[topo]` returns `True` if `topo` is a valid `FCTopology` object or a list thereof.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This is a valid topology: it has an id, a list of propagators, a list of loop and external momenta, a list of possible substitutions for kinematic invariants and an empty list reserved for future applications*)


{FAD[p1],FAD[p2],FAD[p3],FAD[Q-p1-p2-p3],FAD[Q-p1-p2],FAD[Q-p1],FAD[Q-p2],FAD[p1+p3]}


topo=FCTopology[topo1,{FAD[p1],FAD[p2],FAD[p3],FAD[Q-p1-p2-p3],FAD[Q-p1-p2],
FAD[Q-p1],FAD[Q-p2],FAD[p1+p3]},{p1,p2,p3},{Q},{},{}]


FCLoopValidTopologyQ[topo]


(* ::Text:: *)
(*This topology is missing information about loop and external momenta*)


topoWrong=FCTopology[topo1,{FAD[p1],FAD[p2],FAD[Q-p1-p2-p3],FAD[Q-p1-p2],
FAD[Q-p1],FAD[p1+p3]},{},{}]


FCLoopValidTopologyQ[topoWrong]



