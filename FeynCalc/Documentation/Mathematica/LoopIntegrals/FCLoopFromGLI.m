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


(* ::Text:: *)
(*`FCLoopFromGLI` can also handle products of `GLI`s (currently only for standalone integrals or lists of integrals but not for amplitudes).*)
(*In this case it will automatically introduce dummy names for the loop momenta.*)


FCLoopFromGLI[GLI["topoBox1L",{1,0,1,0}]GLI["topoBox1L",{0,1,0,1}],topos]


(* ::Text:: *)
(*You can customize the naming scheme for the momenta via the `LoopMomentum` option. The first argument gives*)
(*the number of the loop integral, while the second corresponds to a particular loop momentum this integral depends on.*)


SelectNotFree[Options[FCLoopFromGLI],LoopMomenta]


FCLoopFromGLI[GLI["topoBox1L",{1,0,1,0}]GLI["topoBox1L",{0,1,0,1}],topos,LoopMomenta->Function[{x,y},
"p"<>ToString[x]<>ToString[x]]]
