(* ::Package:: *)

 


(* ::Section:: *)
(*FeynAmpDenominator*)


(* ::Text:: *)
(*`FeynAmpDenominator[...]` represents the inverse denominators of the propagators, i.e. `FeynAmpDenominator[x]` is $1/x$. Different propagator denominators are represented using special heads such as `PropagatorDenominator`, `StandardPropagatorDenominator`, `CartesianPropagatorDenominator` etc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FAD](FAD.md), [SFAD](SFAD.md), [CFAD](CFAD.md), [GFAD](GFAD.md), [FeynAmpDenominatorSimplify](FeynAmpDenominatorSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*The legacy way to represent standard Lorentzian propagators is to use `PropagatorDenominator`. Here the sign of the mass term is fixed to be $-1$ and no information on the $i \eta$- prescription is available. Furthermore, this way it is not possible to enter eikonal propagators*)


FeynAmpDenominator[PropagatorDenominator[Momentum[p,D],m]]


FeynAmpDenominator[PropagatorDenominator[Momentum[p,D],m],PropagatorDenominator[Momentum[p-q,D],m]]


(* ::Text:: *)
(*It is worth noting that the Euclidean mass dependence still can be introduced via a trick where the mass symbol is multiplied by the imaginary unit $i$*)


FeynAmpDenominator[PropagatorDenominator[Momentum[p,D],I m]]
%//FeynAmpDenominatorExplicit


(* ::Text:: *)
(*The shortcut to enter `FeynAmpDenominator`s with `PropagatorDenominator`s is `FAD`*)


FAD[p]


FAD[{p,m}]


FAD[{p,m,3}]


FeynAmpDenominator[PropagatorDenominator[Momentum[p,D],m]]//FCE//StandardForm


(* ::Text:: *)
(*Since version 9.3, a more flexible input is possible using `StandardPropagatorDenominator`*)


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,-m^2,{1,1}]]


(* ::Text:: *)
(*The mass term can be anything, as long as it does not depend on the loop momenta*)


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,m^2,{1,1}]]


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,-m^2,{1,1}]]


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,SPD[q,q],{1,1}]]


(* ::Text:: *)
(*One can also change the sign of $i \eta$, although currently no internal functions make use of it*)


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,-m^2,{1,-1}]]


(* ::Text:: *)
(*The propagator may also be raised to integer or symbolic powers*)


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,m^2,{3,1}]]


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,m^2,{-2,1}]]


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,m^2,{n,1}]]


(* ::Text:: *)
(*Eikonal propagators are fully supported*)


FeynAmpDenominator[StandardPropagatorDenominator[0,Pair[Momentum[p,D],Momentum[q,D]],-m^2,{1,1}]]


FeynAmpDenominator[StandardPropagatorDenominator[0,Pair[Momentum[p,D],Momentum[q,D]],0,{1,1}]]


(* ::Text:: *)
(*FeynCalc keeps trace of the signs of the scalar products in the eikonal propagators. This is where the  $i \eta$- prescription may come handy*)


FeynAmpDenominator[StandardPropagatorDenominator[0,-Pair[Momentum[p,D],Momentum[q,D]],0,{1,1}]]


FeynAmpDenominator[StandardPropagatorDenominator[0,Pair[Momentum[p,D],Momentum[q,D]],0,{1,-1}]]


(* ::Text:: *)
(*The shortcut to enter `FeynAmpDenominators` with `StandardPropagatorDenominators` is `SFAD`*)


FeynAmpDenominator[StandardPropagatorDenominator[Momentum[p,D],0,-m^2,{1,1}]]//FCE//StandardForm


(* ::Text:: *)
(*Eikonal propagators are entered using the `Dot` (`.`) as in noncommutative products*)


FeynAmpDenominator[StandardPropagatorDenominator[0,Pair[Momentum[p,D],Momentum[q,D]],-m^2,{1,1}]]//FCE//StandardForm


(* ::Text:: *)
(*The Cartesian version of `StandardPropagatorDenominator` is called `CartesianPropagatorDenominator`. The syntax is almost the same as for `StandardPropagatorDenominator`, except that the momenta and scalar products must be Cartesian.*)


FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p,D-1],0,m^2,{1,-1}]]


FeynAmpDenominator[CartesianPropagatorDenominator[0,CartesianPair[CartesianMomentum[p,D-1],CartesianMomentum[q,D-1]],m^2,{1,-1}]]


(* ::Text:: *)
(*The shortcut to enter `FeynAmpDenominators` with `CartesianPropagatorDenominators` is `CFAD`*)


FCE[FeynAmpDenominator[CartesianPropagatorDenominator[CartesianMomentum[p,D-1],0,m^2,{1,-1}]]]//StandardForm


(* ::Text:: *)
(*To represent completely arbitrary propagators one can use `GenericPropagatorDenominator`. However, one should keep in mind that the number of useful manipulations and automatic simplifications available for such propagators is very limited.*)


FeynAmpDenominator[GenericPropagatorDenominator[x,{1,1}]]


(* ::Text:: *)
(*This is a nonlinear propagator that appears in the calculation of the QCD Energy-Energy-Correlation function*)


FeynAmpDenominator[GenericPropagatorDenominator[2 z Pair[Momentum[p1,D],Momentum[Q,D]]Pair[Momentum[p2,D],Momentum[Q,D]]-Pair[Momentum[p1,D],Momentum[p2,D]],{1,1}]]


(* ::Text:: *)
(*The shortcut to enter `FeynAmpDenominator`s with `GenericPropagatorDenominator`s is `GFAD`*)


FeynAmpDenominator[GenericPropagatorDenominator[2 z Pair[Momentum[p1,D],Momentum[Q,D]]Pair[Momentum[p2,D],Momentum[Q,D]]-Pair[Momentum[p1,D],Momentum[p2,D]],{1,1}]]//FCE//StandardForm
