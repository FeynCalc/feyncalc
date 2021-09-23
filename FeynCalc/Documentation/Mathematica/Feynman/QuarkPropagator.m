(* ::Package:: *)

 


(* ::Section:: *)
(*QuarkPropagator*)


(* ::Text:: *)
(*`QuarkPropagator[p]` is the massless quark propagator.*)


(* ::Text:: *)
(*`QuarkPropagator[{p, m}]` gives the quark propagator with mass $m$.*)


(* ::Text:: *)
(*`QP` can be used as an abbreviation of `QuarkPropagator`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GluonPropagator](GluonPropagator.md), [QuarkGluonVertex](QuarkGluonVertex.md).*)


(* ::Subsection:: *)
(*Examples*)


QuarkPropagator[p,Explicit->True]


QuarkPropagator[{p,m},Explicit->True]


QP[{p,m}]
Explicit[%]
