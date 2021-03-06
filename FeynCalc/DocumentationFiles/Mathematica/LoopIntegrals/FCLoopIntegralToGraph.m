(* ::Package:: *)

 


(* ::Section:: *)
(* FCLoopIntegralToGraph *)


(* ::Text:: *)
(*`FCLoopIntegralToGraph[int, {q1, q2, ...}]` constructs a graph representation of the loop integral `int` that depends on the loop momenta `q1, q2, ...`. The function returns a list of the form `{edges,labels,props,pref}`, where `edges` is a list of edge rules representing the loop integral `int`, `labels` is a list of lists containing the line momentum, multiplicity and the mass term of each propagator, `props` is a list with the original propagators and `pref` is the piece of the integral that was ignored when constructing the graph representation (e.g. scalar products or vectors in the numerator) *)


(* ::Text:: *)
(*Use `FCLoopGraphPlot` to visualize the output of `FCLoopIntegralToGraph`*)


(* ::Text:: *)
(*A quick and simple way to plot the graph is to evaluate `GraphPlot[List @@@ Transpose[output[[1 ;; 2]]]]` or `GraphPlot[Labeled @@@ Transpose[output[[1 ;; 2]]]]`. The visual quality will not be that great, though. To obtain a nicer plot one might use `GraphPlot` with a custom `EdgeTaggedGraph` or export the output to a file and visualize it with an external tool such as dot/neato from graphviz.*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*FCLoopGraphPlot.*)


(* ::Subsection:: *)
(* Examples *)


out=FCLoopIntegralToGraph[ FAD[{q-k1},k1,q-k2,k2,{k2-k3,mb},{k1-k3,mb}],{k1,k2,k3}]


FCLoopGraphPlot[out]


Labeled@@@Transpose[out[[1;;2]]]


GraphPlot[List@@@Transpose[out[[1;;2]]]]
