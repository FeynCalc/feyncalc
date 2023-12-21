(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopGetKinematicInvariants*)


(* ::Text:: *)
(*`FCLoopGetKinematicInvariants[topo]` returns the list of kinematic invariants (masses and scalar products) present in the given topology `topo`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md).*)


(* ::Subsection:: *)
(*Examples*)


topo1=FCTopology[topo2,{SFAD[{{l+P/2,0},{mq^2,1},1}],SFAD[{{l-P/2,0},{mq^2,1},1}],SFAD[{{k1+l-P/2,0},{mq^2,1},
1}]},{l},{k1,P},{Hold[Pair][Momentum[k1,D],Momentum[k1,D]]->0,Hold[
Pair][Momentum[P,D],Momentum[q,D]]->0,Hold[Pair][Momentum[P,D],
Momentum[P,D]]->4*mq^2,Hold[Pair][Momentum[k2,D],Momentum[P,D]]->2*mq^
2,Hold[Pair][Momentum[k1,D],Momentum[k2,D]]->2*mq^2},{}]


FCLoopGetKinematicInvariants[topo1]
