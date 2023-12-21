(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopRemovePropagator*)


(* ::Text:: *)
(*`FCLoopRemovePropagator[input,{pos1,pos2,...}]` returns a new `FCTopology` or `GLI` obtained from input by removing propagators at positions listed in `{pos1,pos2,...}`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopCreatePartialFractioningRules](FCLoopCreatePartialFractioningRules.md), [FCTopology](FCTopology.md), [GLI](GLI.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A 2-loop topology with one external momentum `Q`*)


topo=FCTopology[topo1,{SFAD[p1],SFAD[p2],SFAD[Q-p1-p2],SFAD[Q-p2],SFAD[Q-p1]},{p1,p2},{Q},{
Hold[SPD[Q]]->qq},{}]


(* ::Text:: *)
(*The same topology with the 1st and 3rd propagators removed. Notice that the new name is created using the suffix specified via the option `Names`*)


FCLoopRemovePropagator[topo,{1,3}]


gli=GLI[topo2,{1,1,1,2,0,1,1}]


FCLoopRemovePropagator[gli,{2,4}]
