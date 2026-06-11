(* ::Package:: *)

(* ::Section:: *)
(*FCLoopRewriteIncompleteTopologies*)


(* ::Text:: *)
(*`FCLoopRewriteIncompleteTopologies[expr , topos]` handles topologies with incomplete propagator bases in the given expression. The routine will automatically perform basis completions by adding missing propagators, introduce new names for the resulting topologies and return back the expression depending on those new topologies together with a list of the corresponding topologies.*)


(* ::Text:: *)
(*The input expression is expected to be of the form returned by `FCLoopFindTopologies`, e.g. with numerators separated from the denominators where the latter are written as `GLI`s.*)


(* ::Text:: *)
(*The names of the automatically generated topology can be controlled using the `Names` option.*)


(* ::Text:: *)
(*By default the basis completion approach (controlled by the `Method` option)  is set to `Automatic`. This means that the function will use propagators already present in the list of supplied topologies to find complete the bases. It is also possible to specify the propagators explicitly as a list or use the option `ScalarProduct` for automatically adding eikonal propagators.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [FCLoopFindIncompleteTopologies](FCLoopFindIncompleteTopologies.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),*)
(*[SubtopologyMarker](SubtopologyMarker.md).*)


(* ::Subsection:: *)
(*Examples*)


topos={FCTopology[topo1,{SFAD[k1],SFAD[k2]},{k1,k2},{},{},{}]}


expr = FCGV["GLIProduct"][SPD[k1,k2],GLI[topo1,{1,1}]]


FCLoopRewriteIncompleteTopologies[expr,topos,Method->{SFAD[k1-k2]}]


FCLoopRewriteIncompleteTopologies[expr,topos,Method->ScalarProduct]



