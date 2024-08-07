(* ::Package:: *)

 


(* ::Section:: *)
(*FromGFAD*)


(* ::Text:: *)
(*`FromGFAD[exp]` converts all suitable generic propagator denominators into standard and Cartesian propagator denominators.*)


(* ::Text:: *)
(*The options `InitialSubstitutions` and `IntermediateSubstitutions` can be used to help the function handle nontrivial propagators. In particular,  `InitialSubstitutions` can define rules for completing the square in the loop momenta of the propagator, while `IntermediateSubstitutions` contains relations for scalar products appearing in those rules.*)


(* ::Text:: *)
(*Another useful option is `LoopMomenta` which is particularly helpful when converting mixed quadratic-eikonal propagators to quadratic ones.*)


(* ::Text:: *)
(*For propagators containing symbolic variables it might be necessary to tell the function that those are larger than zero (if applicable), so that expressions such as $\sqrt{\lambda^2}$ can be simplified accordingly. To that aim one should use the option `PowerExpand`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [GFAD](GFAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [FeynAmpDenominatorExplicit](FeynAmpDenominatorExplicit.md).*)


(* ::Subsection:: *)
(*Examples*)


GFAD[SPD[p1]]

ex=FromGFAD[%]


ex//StandardForm


ex=GFAD[SPD[p1]+2 SPD[p1,p2]]


FromGFAD[ex]


(* ::Text:: *)
(*We can get a proper conversion into a quadratic propagator using the option `LoopMomenta`. Notice that here `p2.p2` is being put into the mass slot*)


FromGFAD[ex,LoopMomenta->{p1}]


ex//StandardForm


GFAD[{{CSPD[p1]+2 CSPD[p1,p2]+m^2,-1},2}]

ex=FromGFAD[%]


ex//StandardForm


DataType[la,FCVariable]=True;
prop=FeynAmpDenominator[GenericPropagatorDenominator[-la Pair[Momentum[p1,D],
Momentum[p1,D]]+2 Pair[Momentum[p1,D],Momentum[q,D]],{1,1}]]


ex=FromGFAD[prop]


ex=FromGFAD[prop,LoopMomenta->{p1}]


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


(* ::Text:: *)
(*Notice that `FromGFAD` does not expand scalar products in the propagators before trying to convert*)
(*them to `SFAD`s or `CFAD`s. If this is needed, the user should better apply `ExpandScalarProduct` to the expression by hand.*)


FromGFAD[ex]


FromGFAD[ExpandScalarProduct[ex]]


(* ::Text:: *)
(*Using the option `InitialSubstitutions` one can perform certain replacement that might not be found automatically. The values of scalar products can be set using `IntermediateSubstitutions`*)


ex=GFAD[{{SPD[k1, k1] - 2*gkin*meta*u0b*SPD[k1, n], 1}, 1}];


(* ::Text:: *)
(*Notice that we need to declare the appearing variables as `FCVariable`s*)


(DataType[#,FCVariable]=True)&/@{gkin,meta,u0b};


(* ::Text:: *)
(*Without these options we get a mixed quadratic-eikonal propagator that will cause us troubles when doing topology minimizations.*)


FromGFAD[ex,FCE->True]
%//InputForm


(* ::Text:: *)
(*But when doing everything right we end up with a purely quadratic propagator*)


FromGFAD[ex,InitialSubstitutions->{ExpandScalarProduct[SPD[k1-gkin meta u0b n]]->SPD[k1-gkin meta u0b n]},
IntermediateSubstitutions->{SPD[n]->0,SPD[nb]->0,SPD[n,nb]->2}]


(* ::Text:: *)
(*However, in this case the function can also figure out the necessary square completion on its own if we tell it that `k1` is a momentum w.r.t which the square should be completed. In this case the option `IntermediateSubstitutions`  is not really needed*)


FromGFAD[ex,LoopMomenta->{k1}]


(* ::Text:: *)
(*It is still helpful, though*)


FromGFAD[ex,LoopMomenta->{k1},IntermediateSubstitutions->{SPD[n]->0,SPD[nb]->0,SPD[n,nb]->2}]


(* ::Text:: *)
(*If we have multiple loop momenta, we need to first complete the square with respect to them before handling the full expression*)


ex=GFAD[{{SPD[k1,k1]+2 SPD[k1,k2]+SPD[k2,k2]+2 gkin meta (SPD[k1,n]+SPD[k2,n]),1},1}]


FromGFAD[ex,LoopMomenta->{k1,k2}]


FromGFAD[ex,LoopMomenta->{k1,k2},
InitialSubstitutions->{ExpandScalarProduct[SPD[k1+k2]]->SPD[k1+k2]},
IntermediateSubstitutions->{SPD[n]->0}]
