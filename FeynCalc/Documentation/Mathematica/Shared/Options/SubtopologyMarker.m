(* ::Package:: *)

(* ::Section:: *)
(*SubtopologyMarker*)


(* ::Text:: *)
(*`SubtopologyMarker` is an option for `FCLoopFindTopologies`, `FCLoopFindTopologyMappings` and other topology related functions. It denotes the symbol that is used to specify that the given topology is a subtopology of another topology and has*)
(*been obtained by removing some of the original propagators (i.e. there are no momenta shifts involved)*)


(* ::Text:: *)
(*This information must be put into the very last list of the FCTopology object describing the corresponding subtopology. The syntax is `marker->topoID` where `topoID` is the ID of the larger topology.*)


(* ::Text:: *)
(*Setting `SubtopologyMarker` to `False` means that the information about subtopologies will not be added when generating subtopologies and will be ignored by routines related to topology mappings.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopFindTopologies](FCLoopFindTopologies.md), [FCLoopFindTopologyMappings](FCLoopFindTopologyMappings.md),*)
(*[FCLoopFindSubtopologies](FCLoopFindSubtopologies.md).*)


(* ::Subsection:: *)
(*Examples*)
