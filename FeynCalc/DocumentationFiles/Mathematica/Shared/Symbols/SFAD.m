(* ::Package:: *)

 


(* ::Section:: *)
(*SFAD*)


(* ::Text:: *)
(*`SFAD[{{q1 +..., p1 . q2 +...,} {m^2, s}, n}, ...]` denotes a Cartesian propagator given by \frac{1}{[(q_1+\ldots)^2 + p_1 \cdot q_2 ... + m^2 + s i \eta]^n}, where $q_1^2$ and $p_1 \cdot q_2$ are Cartesian scalar products in $D-1$ dimensions.*)


(* ::Text:: *)
(*For brevity one can also use shorter forms such as `SFAD[{q1+ ...,  m^2}, ...]`, `SFAD[{q1+ ...,  m^2 , n}, ...]`, `SFAD[{q1+ ...,  {m^2, -1}}, ...]`, `SFAD[q1,...]`  etc.*)


(* ::Text:: *)
(*If `s` is not explicitly specified, its value is determined by the option `EtaSign`, which has the default value `+1`.*)


(* ::Text:: *)
(*If `n` is not explicitly specified, then the default value `1` is assumed. Translation into FeynCalcI internal form is performed by `FeynCalcInternal`, where a `SFAD` is encoded using the special head `CartesianPropagatorDenominator`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FAD](FAD.md), [GFAD](GFAD.md), [CFAD](CFAD.md).*)


(* ::Subsection:: *)
(*Examples*)


SFAD[{{p,0},m^2}]


SFAD[{{p,0},{m^2,-1}}]


SFAD[{{p,0},{-m^2,-1}}]


SFAD[{{0,p . q},m^2}]
