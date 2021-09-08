(* ::Package:: *)

 


(* ::Section:: *)
(*GFAD*)


(* ::Text:: *)
(*`GFAD[{{{x, s}, n}, ...]` denotes a generic propagator given by $\frac{1}{[x + s i \eta]^n}$, where `x` can be an arbitrary expression. For brevity one can also use shorter forms such as `GFAD[{x, n}, ...]`, `GFAD[{x}, ...]` or `GFAD[x, ...]`.*)


(* ::Text:: *)
(*If s is not explicitly specified, then its value is determined by the option `EtaSign`, which has the default value `+1`.*)


(* ::Text:: *)
(*If `n` is not explicitly specified, then the default value `1` is assumed. Translation into FeynCalc internal form is performed by `FeynCalcInternal`, where a `GFAD` is encoded using the special head `GenericPropagatorDenominator`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FAD](FAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md).*)


(* ::Subsection:: *)
(*Examples*)


GFAD[2z SPD[p1,q] SPD[p2,q]+ x SPD[p1,p2]]
FeynAmpDenominatorExplicit[%]
%//FCE//StandardForm
