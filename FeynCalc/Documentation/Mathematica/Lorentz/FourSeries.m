(* ::Package:: *)

 


(* ::Section:: *)
(*FourSeries*)


(* ::Text:: *)
(*`FourSeries[exp, {p,p0,n}]` calculates Taylor series of `exp` w.r.t the $4$-vector $p$ to $n$th order.*)
(*If the expression diverges at $p = p_0$, it will be returned unevaluated.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FourDivergence](FourDivergence.md), [ThreeDivergence](ThreeDivergence.md).*)


(* ::Subsection:: *)
(*Examples*)


(m^2+SPD[p,q])FAD[{k},{k+p}]

FourSeries[%,{p,0,2}]


(SPD[p,q])DiracTrace[GAD[mu] . GSD[p+q] . GAD[nu]]

FourSeries[%,{p,0,2}]



