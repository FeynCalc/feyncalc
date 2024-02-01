(* ::Package:: *)

 


(* ::Section:: *)
(*FromGFAD*)


(* ::Text:: *)
(*`FromGFAD[exp]` converts all suitable generic propagator denominators into standard and Cartesian propagator denominators.*)


(* ::Text:: *)
(*The options `InitialSubstitutions` and `IntermediateSubstitutions` can be used to help the function handle nontrivial propagators.*)


(* ::Text:: *)
(*For propagators containing symbolic variables it might be necessary to tell the function that those are larger than zero (if applicable), so that expressions such as $\sqrt{\lambda^2}$ can be simplified accordingly.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GFAD](GFAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit.md).*)


(* ::Subsection:: *)
(*Examples*)


GFAD[SPD[p1]]

ex=FromGFAD[%]


ex//StandardForm


GFAD[SPD[p1]+2 SPD[p1,p2]]

ex=FromGFAD[%]


ex//StandardForm


GFAD[{{CSPD[p1]+2 CSPD[p1,p2]+m^2,-1},2}]

ex=FromGFAD[%]


ex//StandardForm


prop=FeynAmpDenominator[GenericPropagatorDenominator[-la Pair[Momentum[p1,D],
Momentum[p1,D]]+2 Pair[Momentum[p1,D],Momentum[q,D]],{1,1}]]


ex=FromGFAD[prop]


ex//StandardForm


ex=FromGFAD[prop,PowerExpand->{la}]


ex//StandardForm


ex=GFAD[{{-SPD[p1, p1], 1}, 1}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 1}]*
GFAD[{{-SPD[p3, p3], 1}, 1}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*
SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}] +  (-2*mg^2*GFAD[{{-SPD[p1, p1], 1}, 2}]*
GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 1}]*GFAD[{{-SPD[p3, p3], 1}, 1}]*
SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}] - 
   2*mg^2*GFAD[{{-SPD[p1, p1], 1}, 1}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 
   1}, 2}]*GFAD[{{-SPD[p3, p3], 1}, 1}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*
   SFAD[{{I*(p3 + q), 0}, {-mb^2, 1}, 1}] -    2*mg^2*GFAD[{{-SPD[p1, p1], 1}, 
   1}]*GFAD[{{SPD[p1, -p1 + 2*p3] - SPD[p3, p3], 1}, 1}]*GFAD[{{-SPD[p3, p3], 
   1}, 2}]*SFAD[{{I*(p1 + q), 0}, {-mb^2, 1}, 1}]*SFAD[{{I*(p3 + q), 0}, 
   {-mb^2, 1}, 1}])/2


FromGFAD[ex]


(* ::Text:: *)
(*Using the option `InitialSubstitutions` one can perform certain replacement that might not be found automatically*)


ex=GFAD[SPD[k1]+2SPD[k1,k2]+SPD[k2]+SPD[k1,n]]


FromGFAD[ex,FCE->True]
%//InputForm


FromGFAD[ex,FCE->True,InitialSubstitutions->{ExpandScalarProduct[SPD[k1+k2]]->SPD[k1+k2]}]
%//InputForm
