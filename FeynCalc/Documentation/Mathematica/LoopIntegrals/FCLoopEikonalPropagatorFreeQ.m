(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopEikonalPropagatorFreeQ*)


(* ::Text:: *)
(*`FCLoopEikonalPropagatorFreeQ[exp]` checks if the integral is free of eikonal propagators $\frac{1}{p \cdot q+x}$. If the option `First` is set to `False`, propagators that have both a quadratic and linear piece, e.g. $\frac{1}{p^2 + p \cdot q+x}$ will also count as eikonal propagators. The option `Momentum` can be used to check for the presence of eikonal propagators only with respect to particular momenta. The check is performed only for `StandardPropagatorDenominator` and `CartesianPropagatorDenominator`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


FCI@SFAD[p,p-q]
FCLoopEikonalPropagatorFreeQ[%]


FCI@SFAD[{{0,p . q}}]
FCLoopEikonalPropagatorFreeQ[%]


FCI@CFAD[{{0,p . q}}]
FCLoopEikonalPropagatorFreeQ[%,Momentum->{q}]


FCI@SFAD[{{q,q . p}}]
FCLoopEikonalPropagatorFreeQ[%,First->False]
