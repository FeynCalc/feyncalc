(* ::Package:: *)

(* ::Section:: *)
(*FCLoopRewriteOverdeterminedTopologies*)


(* ::Text:: *)
(*`FCLoopRewriteOverdeterminedTopologies[expr , topos]` handles topologies with overdetermined propagator bases in the given expression. The routine will automatically perform partial fraction decomposition on the affected topologies, introduce new names for the resulting topologies and return back the expression depending on those new topologies together with a list of the corresponding topologies.*)


(* ::Text:: *)
(*The input expression is expected to be of the form returned by `FCLoopFindTopologies`, e.g. with numerators separated from the denominators where the latter are written as `GLI`s.*)


(* ::Text:: *)
(*The names of the automatically generated topology can be controlled using the `Names` option.*)


(* ::Text:: *)
(*Notice that the returned topologies can be related to each other, while some of them may even have identical sets of propagators. This is expected, because the output of this function usually gets passed to `FCLoopFindTopologyMappings`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindOverdeterminedTopologies](FCLoopFindOverdeterminedTopologies.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),*)
(*[SubtopologyMarker](SubtopologyMarker.md).*)


(* ::Subsection:: *)
(*Examples*)


topos={FCTopology[topo1,{SFAD[k1],SFAD[k1+p],SFAD[k1-p]},{k1},{p},{},{}]}


expr = FCGV["GLIProduct"][SPD[k1,p],GLI[topo1,{1,1,1}]]


FCLoopRewriteOverdeterminedTopologies[expr,topos]
