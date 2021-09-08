(* ::Package:: *)

 


(* ::Section:: *)
(*CartesianMomentum*)


(* ::Text:: *)
(*`CartesianMomentum[p]` is the head of a 3-momentum `p`. The internal representation of a $3$-dimensional `p` is `CartesianMomentum[p]`. For other than three dimensions: `CartesianMomentum[p, Dimension]`. `CartesianMomentum[p, 3]` simplifies to `CartesianMomentum[p]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Momentum](Momentum.md), [TemporalMomentum](TemporalMomentum.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This is a 3-dimensional momentum*)


CartesianMomentum[p]


(* ::Text:: *)
(*As an optional second argument the dimension must be specified if it is different from 3*)


CartesianMomentum[p,D-1]


(* ::Text:: *)
(*The dimension index is suppressed in the output.*)


CartesianMomentum[p,d-1]


a1=CartesianMomentum[-q]
a1//StandardForm


a2 = CartesianMomentum[p-q] + CartesianMomentum[2q]
a2//StandardForm


a2//MomentumExpand//StandardForm


a2//MomentumCombine//StandardForm


(* ::Text:: *)
(*Notice that when changing the dimension, one must specify its value as if the the 3-vector were the spatial component of the corresponding 4-vector*)


ChangeDimension[CartesianMomentum[p],d-1]//StandardForm


ChangeDimension[CartesianMomentum[p],d]//StandardForm


Clear[a1,a2]
