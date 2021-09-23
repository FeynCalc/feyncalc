(* ::Package:: *)

 


(* ::Section:: *)
(*SetTemporalComponent*)


(* ::Text:: *)
(*`SetTemporalComponent[p, val]` sets the value of the temporal component of a $4$-vector $p$, `TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[p]]` to `val`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [TC](TC.md), [TemporalPair](TemporalPair.md), [TemporalMomentum](TemporalMomentum.md).*)


(* ::Subsection:: *)
(*Examples*)


FCClearScalarProducts[]
ClearAll[t]
SetTemporalComponent[p,t]
TC[p]


TC[p+q]//ExpandScalarProduct


SP[p,q]//LorentzToCartesian
