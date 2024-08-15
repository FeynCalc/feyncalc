(* ::Package:: *)

 


(* ::Section:: *)
(*SFAD*)


(* ::Text:: *)
(*`SFAD[{{q1 +..., p1 . q2 +...,} {m^2, s}, n}, ...]` denotes a standard Lorentzian  propagator given by $\frac{1}{[(q_1+\ldots)^2 + p_1 \cdot q_2 ... + m^2 + s i \eta]^n}$, where $q_1^2$ and $p_1 \cdot q_2$ are Lorentzian scalar products in $D$ dimensions.*)


(* ::Text:: *)
(*For brevity one can also use shorter forms such as `SFAD[{q1+ ...,  m^2}, ...]`, `SFAD[{q1+ ...,  m^2 , n}, ...]`, `SFAD[{q1+ ...,  {m^2, -1}}, ...]`, `SFAD[q1,...]` etc.*)


(* ::Text:: *)
(*If `s` is not explicitly specified, its value is determined by the option `EtaSign`, which has the default value `+1` and corresponds to $+ i \eta$*)


(* ::Text:: *)
(*If `n` is not explicitly specified, then the default value `1` is assumed. Translation into the FeynCalc internal form is performed by `FeynCalcInternal`, where an `SFAD` is encoded using the special head `StandardPropagatorDenominator`.*)


(* ::Text:: *)
(*`SFAD` can represent more versatile propagators as compared to the old `FAD`. In particular, `FAD` does not allow one to enter eikonal propagators, track the sign of the $i \eta$ or change the sign and the form of the mass term.*)


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


SFAD[{{0,n . q}}]


SFAD[{{p,p . q},m^2}]


(* ::Text:: *)
(*The so called Smirnov-notation for propagators can be achieved by multiplying the quadratic part by `I` and switching the sign of the mass term.*)


SFAD[{{I*p,0},-m^2}]


(* ::Text:: *)
(*If one wants to have additional variables multiplying loop or external momenta, those need to be declared to be of the `FCVariable` type*)


DataType[la,FCVariable]=True


SFAD[{{0,la p . q},m^2}]


%//FCI//StandardForm
