(* ::Package:: *)

 


(* ::Section:: *)
(*GluonPropagator*)


(* ::Text:: *)
(*`GluonPropagator[p, {\[Mu], a}, {\[Nu], b}]` or `GluonPropagator[p, \[Mu], a, \[Nu], b]` yields the gluon propagator.*)


(* ::Text:: *)
(*`GluonPropagator[p, {\[Mu]}, {\[Nu]}]` or `GluonPropagator[p, \[Mu], \[Nu]]` omits the `SUNDelta`.*)


(* ::Text:: *)
(*`GP` can be used as an abbreviation of `GluonPropagator`.*)


(* ::Text:: *)
(*The gauge and the dimension are determined by the options `Gauge` and `Dimension`. The following settings of `Gauge` are possible:*)


(* ::Text:: *)
(* `1` for the Feynman gauge;  *)
(*`alpha` for the general covariant gauge;*)
(* `Momentum[n] ,1}` for the axial gauge.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GluonSelfEnergy](GluonSelfEnergy.md), [GluonVertex](GluonVertex.md), [GhostVertex](GhostVertex.md), [GhostPropagator](GhostPropagator.md), [GluonGhostVertex](GluonGhostVertex.md).*)


(* ::Subsection:: *)
(*Examples*)


GluonPropagator[p,\[Mu],a,\[Nu],b]
Explicit[%]


GP[p,\[Mu],a,\[Nu],b,Gauge->\[Alpha]]
Explicit[%]


GluonPropagator[p,\[Mu],a,\[Nu],b,Gauge->{Momentum[n],1},Explicit->True]


GP[p,\[Mu],\[Nu]]


Explicit[%]


GluonPropagator[p,\[Mu],a,\[Nu],b,CounterTerm-> 1]//Explicit


GluonPropagator[p,\[Mu],a,\[Nu],b,CounterTerm-> 2]//Explicit


GluonPropagator[p,\[Mu],a,\[Nu],b,CounterTerm-> 3]//Explicit


GluonPropagator[p,\[Mu],a,\[Nu],b,CounterTerm-> 4]//Explicit


GluonPropagator[p,\[Mu],a,\[Nu],b,CounterTerm-> 5]//Explicit
