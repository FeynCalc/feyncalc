(* ::Package:: *)

 


(* ::Section:: *)
(*CounterT*)


(* ::Text:: *)
(*`CounterT` is a factor used by `GluonPropagator` and `QuarkPropagator` when `CounterTerms` is set to `All`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [CounterTerm](CounterTerm.md), [GluonPropagator](GluonPropagator.md), [QuarkPropagator](QuarkPropagator.md).*)


(* ::Subsection:: *)
(*Examples*)


GluonPropagator[p,\[Mu],a,\[Nu],b,Explicit->True,CounterTerm-> All]
