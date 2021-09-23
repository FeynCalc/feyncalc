(* ::Package:: *)

(* ::Section:: *)
(*FCLoopFromGLI*)


(* ::Text:: *)
(*`FCLoopFromGLI[exp, topologies]` replaces `GLI`s in `exp` with the corresponding loop integrals in the `FeynAmpDenominator` notation according to the information provided in topologies.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopValidTopologyQ](FCLoopValidTopologyQ.md).*)


(* ::Subsection:: *)
(*Examples*)


topos={
FCTopology["topoBox1L",{FAD[{q,m0}],FAD[{q+p1,m1}],FAD[{q+p2,m2}],FAD[{q+p2,m3}]},{q},{p1,p2,p3},{},{}],
FCTopology["topoTad2L",{FAD[{q1,m1}],FAD[{q2,m2}],FAD[{q1-q2,0}]},{q1,q2},{},{},{}]}


exp=a1 GLI["topoBox1L",{1,1,1,1}]+a2 GLI["topoTad2L",{1,2,2}]


FCLoopFromGLI[exp,topos]


(* ::Text:: *)
(*Notice that it is necessary to specify all topologies present in `exp`. The function will not accept `GLI`s*)
(*defined for unknown topologies*)


FCLoopFromGLI[GLI["topoXYZ",{1,1,1,1,1}],topos]



