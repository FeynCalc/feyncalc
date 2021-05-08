 
(* ::Section:: *)
(* CFAD *)
(* ::Text:: *)
(*CFAD[{{q1 +..., p1 . q2 +...,} {m^2, s}, n}, ...] denotes a Cartesian propagator given by 1/[(q1+...)^2 + p1.q2 ... + m^2 + sign*I*eta]^n, where q1^2 and p1.q2 are Cartesian scalar products in D-1 dimensions. For brevity one can also use shorter forms such as CFAD[{q1+ ...,  m^2}, ...], CFAD[{q1+ ...,  m^2 , n}, ...], CFAD[{q1+ ...,  {m^2, -1}}, ...], CFAD[q1,...]  etc. If s is not explicitly specified, then its value is determined by the option EtaSign, which has the default value +1. If n is not explicitly specified, then the default value 1 is assumed. Translation into FeynCalc internal form is performed by FeynCalcInternal, where a CFAD is encoded using the special head CartesianPropagatorDenominator..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*FAD, SFAD, GFAD, FeynAmpDenominator.*)



(* ::Subsection:: *)
(* Examples *)




CFAD[{{p,0},m^2}]

FeynAmpDenominatorExplicit[%]

CFAD[{{p,0},{m^2,1}}]

FeynAmpDenominatorExplicit[%]

CFAD[{{p,0},-m^2}]

FeynAmpDenominatorExplicit[%]

CFAD[{{0,p.q},m^2}]

FeynAmpDenominatorExplicit[%]

CFAD[{{0,p.q}}]

FeynAmpDenominatorExplicit[%]
