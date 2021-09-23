(* ::Package:: *)

 


(* ::Section:: *)
(*GluonSelfEnergy*)


(* ::Text:: *)
(*`GluonSelfEnergy[{\[Mu], a}, {\[Nu], b}]` yields the 1-loop gluon self-energy.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GluonPropagator](GluonPropagator.md), [GluonGhostVertex](GluonGhostVertex.md), [GluonVertex](GluonVertex.md), [GhostPropagator](GhostPropagator.md).*)


(* ::Subsection:: *)
(*Examples*)


GluonSelfEnergy[{\[Mu],a},{\[Nu],b},Momentum->p]


GluonSelfEnergy[{\[Mu],a},{\[Nu],b},Gauge->\[Xi],Momentum->q]
