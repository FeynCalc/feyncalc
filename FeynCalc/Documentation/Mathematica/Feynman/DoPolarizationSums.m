(* ::Package:: *)

 


(* ::Section:: *)
(*DoPolarizationSums*)


(* ::Text:: *)
(*`DoPolarizationSums[exp, k, ...]` acts on an expression `exp` that must contain a polarization vector $\varepsilon(k)$  and its complex conjugate (e.g. `exp` can be a matrix element squared).*)


(* ::Text:: *)
(*Depending on the arguments of the function, it will perform a sum over the polarization of $\varepsilon(k)$ and its c.c.*)


(* ::Text:: *)
(*- `DoPolarizationSums[exp, k]` sums over the three physical polarizations of an external massive vector boson with the $4$-momentum `k` and the mass $k^2$.*)
(*- `DoPolarizationSums[exp, k, 0]` replaces the polarization sum of an external massless vector boson with the momentum `k` by $-g^{\mu \nu}$.*)
(*This corresponds to the summation over all 4 polarizations, including the unphysical ones.*)
(*- `DoPolarizationSums[exp, k, n]` sums over physical (transverse) polarizations of an external massless vector boson with the momentum `k`, where `n` is an auxiliary 4-vector from the gauge-dependent polarization sum formula.*)


(* ::Text:: *)
(*Cf. `PolarizationSum` for more examples and explanations on different polarizations.*)


(* ::Text:: *)
(*`DoPolarizationSums` also work with $D$-dimensional amplitudes.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Polarization](Polarization.md), [PolarizationSum](PolarizationSum.md), [NumberOfPolarizations](NumberOfPolarizations.md), [VirtualBoson](VirtualBoson.md), [Uncontract](Uncontract.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*The standard formula for massless vector bosons is valid for all types of the corresponding particles, including gluons.*)


FCClearScalarProducts[]
SP[p]=0;
Pair[LorentzIndex[\[Mu]],Momentum[Polarization[p,-I]]] Pair[LorentzIndex[\[Nu]],
Momentum[Polarization[p,I]]]


DoPolarizationSums[%,p,n]


(* ::Text:: *)
(*In QED the gauge invariance ensures the cancellation of the unphysical polarizations so that for photons one can also employ the simpler replacement with the metric tensor.*)


FCClearScalarProducts[]
SP[p]=0;
Pair[LorentzIndex[\[Mu]],Momentum[Polarization[p,-I]]] Pair[LorentzIndex[\[Nu]],
Momentum[Polarization[p,I]]]


DoPolarizationSums[%,p,0]


(* ::Text:: *)
(*You can also use this trick in QCD, provided that the unphysical degrees of freedom are subtracted using ghosts at a later stage.*)


(* ::Text:: *)
(*Notice that in this case you should not make the polarization vectors transverse using the `Transversality` option.*)


(* ::Text:: *)
(*Furthermore, the averaging over the polarizations of the initial gluons must be done on the physical amplitude squared, i.e. after the ghost contributions have been subtracted.*)


(* ::Text:: *)
(*Massive vector bosons (e.g. W or Z) have 3 degrees of freedom and require no auxiliary vector.*)


FCClearScalarProducts[]
SP[p]=m^2;
Pair[LorentzIndex[\[Mu]],Momentum[Polarization[p,-I]]] Pair[LorentzIndex[\[Nu]],
Momentum[Polarization[p,I]]]


DoPolarizationSums[%,p]


(* ::Text:: *)
(*A more realistic example of summing over the polarizations of the photons in $e^+e^ \to  \gamma \gamma$*)


ClearAll[s,t,u];
FCClearScalarProducts[];
SP[k1]=0;
SP[k2]=0;
amp=(-((Spinor[Momentum[p1],0,1] . GS[Polarization[k1,I,
Transversality->True]] . GS[k2-p2] . GS[Polarization[k2,I,
Transversality->True]] . Spinor[-Momentum[p2],0,1]*SMP["e"]^2)/t)-
(Spinor[Momentum[p1],0,1] . GS[Polarization[k2,I,
Transversality->True]] . GS[k1-p2] . GS[Polarization[k1,I,
Transversality->True]] . Spinor[-Momentum[p2],0,1]*SMP["e"]^2)/u)*
(-((Spinor[-Momentum[p2],0,1] . GS[Polarization[k1,-I,
Transversality->True]] . GS[k1-p2] . GS[Polarization[k2,-I,
Transversality->True]] . Spinor[Momentum[p1],0,1]*
SMP["e"]^2)/u)-(Spinor[-Momentum[p2],0,1] . GS[Polarization[k2,-I,
Transversality->True]] . GS[k2-p2] . GS[Polarization[k1,-I,
Transversality->True]] . Spinor[Momentum[p1],0,1]*SMP["e"]^2)/t)


amp//DoPolarizationSums[#,k1,0]&//DoPolarizationSums[#,k2,0]&


(* ::Text:: *)
(*This is a small piece of the matrix element squared for $g g to  Q \bar{Q}$.*)


(* ::Text:: *)
(*The proper summation over the polarizations of the gluons requires a choice of two auxiliary vectors (unless we subtract the unphysical contributions using ghosts).*)


(* ::Text:: *)
(*It is customary to take the 4-momentum of another gluon as the auxiliary vector in the summation formula.*)


(* ::Text:: *)
(*The option `ExtraFactor` is used to average over the polarizations of the initial gluons.*)


ClearAll[s,t,u];
FCClearScalarProducts[];
SP[p1]=0;
SP[p2]=0;


amp=1/(s^2 SUNN (1-SUNN^2) u^2) 2 SMP["g_s"]^4 SP[k1,
Polarization[p2,-I,Transversality->True]] SP[k1,Polarization[p2,
I,Transversality->True]] (2 s^2 SP[k1,Polarization[p1,I,
Transversality->True]] SP[k2,Polarization[p1,-I,
Transversality->True]]+2 s SUNN^2 t SP[k1,Polarization[p1,I,
Transversality->True]] SP[k2,Polarization[p1,-I,
Transversality->True]]+s SUNN^2 u SP[k1,Polarization[p1,I,
Transversality->True]] SP[k2,Polarization[p1,-I,
Transversality->True]]+2 s^2 SP[k1,Polarization[p1,-I,
Transversality->True]] SP[k2,Polarization[p1,I,
Transversality->True]]+2 s SUNN^2 t SP[k1,Polarization[p1,-I,
Transversality->True]] SP[k2,Polarization[p1,I,
Transversality->True]]+s SUNN^2 u SP[k1,Polarization[p1,-I,
Transversality->True]] SP[k2,Polarization[p1,I,
Transversality->True]]+2 SUNN^2 u^2 SP[k2,
Polarization[p1,-I,Transversality->True]] SP[k2,
Polarization[p1,I,Transversality->True]])


amp//DoPolarizationSums[#,p1,p2,ExtraFactor -> 1/2]&//
DoPolarizationSums[#,p2,p1,ExtraFactor -> 1/2]&//Simplify


(* ::Text:: *)
(*We can also do the same calculation in $D$-dimensions*)


ClearAll[s,t,u];
FCClearScalarProducts[];
SPD[p1]=0;
SPD[p2]=0;
ChangeDimension[amp,D]//DoPolarizationSums[#,p1,p2,ExtraFactor -> 1/2]&//
DoPolarizationSums[#,p2,p1,ExtraFactor -> 1/2]&//Simplify


(* ::Text:: *)
(*`DoPolarizationSums` will complain if you try to sum over the polarizations of a massless vector boson that is not on-shell*)


PolarizationVector[p,mu]ComplexConjugate[PolarizationVector[p,mu]]
DoPolarizationSums[%,p,0]


(* ::Text:: *)
(*The obvious solution to remove this warning is to put the boson on-shell*)


FCClearScalarProducts[]
ScalarProduct[p,p]=0
PolarizationVector[p,mu]ComplexConjugate[PolarizationVector[p,mu]]
DoPolarizationSums[%,p,0]


(* ::Text:: *)
(*However, if you have a massless virtual boson in the final state that by definition cannot be on-shell, (e.g. in the process $q \bar{q} \to g \gamma^\ast$), you can tell this to the function by setting the option `VirtualBoson` to `True`.*)


FCClearScalarProducts[]
PolarizationVector[p,mu]ComplexConjugate[PolarizationVector[p,mu]]
DoPolarizationSums[%,p,0,VirtualBoson->True]


(* ::Text:: *)
(*It may happen that your expression is not directly proportional to a pair of polarization vectors. In this case terms that are free of polarization vectors will be multiplied by the suitable number of polarizations. This behavior is controlled by the option `NumberOfPolarizations`. The default value `Automatic` means that the function will automatically figure out the correct number of polarizations.*)


(* ::Text:: *)
(*Here we have 2 physical polarizations (massless vector boson)*)


FCClearScalarProducts[];
ScalarProduct[p,p]=0;
PolarizationVector[p,mu]ComplexConjugate[PolarizationVector[p,mu]]+xyz
DoPolarizationSums[%,p,n]


(* ::Text:: *)
(*In $D$ dimensions the number of polarizations becomes $D-2$*)


FCClearScalarProducts[];
ScalarProduct[p,p]=0;
ChangeDimension[PolarizationVector[p,mu]*
ComplexConjugate[PolarizationVector[p,mu]]+xyz,D]
DoPolarizationSums[%,p,n]


(* ::Text:: *)
(*A massive vector boson has 3 physical polarizations in 4 dimensions*)


FCClearScalarProducts[];
ScalarProduct[p,p]=M^2;
PolarizationVector[p,mu]ComplexConjugate[PolarizationVector[p,mu]]+xyz
DoPolarizationSums[%,p]


(* ::Text:: *)
(*or $D-1$ physical polarizations in $D$ dimensions*)


FCClearScalarProducts[];
ScalarProduct[p,p]=M^2;
ChangeDimension[PolarizationVector[p,mu]*
ComplexConjugate[PolarizationVector[p,mu]]+xyz,D]
DoPolarizationSums[%,p]


(* ::Text:: *)
(*In the case of a standalone expression that contains no polarization vectors whatsoever, the function*)
(*has no way to determine the correct number of polarizations.*)


DoPolarizationSums[xyz,p,n]


(* ::Text:: *)
(*Here additional user input is needed*)


DoPolarizationSums[xyz,p,NumberOfPolarizations->2]



