(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopAddEdgeTags*)


(* ::Text:: *)
(*`FCLoopAddEdgeTags[edges_List, labels_List]` adds user-defined styles and labels to the given edges using the provided list of labels. Styles and labels are attached using the replacement rules provided via the `Style` and `Labeled` options.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopGraphPlot](FCLoopGraphPlot.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*If you use `FCLoopIntegralToGraph` for visualizing loop integrals, the first two entries of its output can be used as the input for FCLoopAddEdgeTags, e.g.  *)


FCLoopAddEdgeTags[FCLoopIntegralToGraph[FAD[p, p - k], {p}][[1 ;; 2]]]
GraphPlot[%]


(* ::Text:: *)
(*If you just want to plot the obtained graph, it is easier to process the output of `FCLoopIntegralToGraph` directly with `FCLoopGraphPlot`, which internally uses `FCLoopAddEdgeTags`.*)


FCLoopIntegralToGraph[FAD[p, p - k], {p}]
FCLoopGraphPlot[%]



