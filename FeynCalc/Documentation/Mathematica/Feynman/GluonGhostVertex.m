(* ::Package:: *)

 


(* ::Section:: *)
(*GluonGhostVertex*)


(* ::Text:: *)
(*`GluonGhostVertex[{p, mu, a}, {q, nu, b}, {k, rho, c}]` or `GluonGhostVertex[ p, mu, a , q, nu, b , k, rho, c]` yields the Gluon-Ghost vertex. The first argument represents the gluon and the third argument the outgoing ghost field (but incoming 4-momentum).*)


(* ::Text:: *)
(*`GGV` can be used as an abbreviation of `GluonGhostVertex`.The dimension and the name of the coupling constant are determined by the options `Dimension` and `CouplingConstant`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GluonPropagator](GluonPropagator.md), [GluonSelfEnergy](GluonSelfEnergy.md), [GluonVertex](GluonVertex.md), [QCDFeynmanRuleConvention](QCDFeynmanRuleConvention.md), [GhostPropagator](GhostPropagator.md).*)


(* ::Subsection:: *)
(*Examples*)


GluonGhostVertex[{p,\[Mu],a},{q,\[Nu],b},{k,\[Rho],c}] 
Explicit[%]
