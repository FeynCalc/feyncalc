(* ::Package:: *)

 


(* ::Section:: *)
(*DoPolarizationSums*)


(* ::Text:: *)
(*`DoPolarizationSums[exp, k, n]` sums over physical (transverse) polarizations of an external massless vector boson with the momentum `k`, where `n` is an auxiliary 4-vector from the gauge-dependent polarization sum formula.*)


(* ::Text:: *)
(*`DoPolarizationSums[exp, k, 0]` replaces the polarization sum of an external massless vector boson with the momentum `k` by $-g^{\mu \nu }$. This corresponds to the summation over all 4 polarizations, including the unphysical ones.*)


(* ::Text:: *)
(*`DoPolarizationSums[exp, k]` sums over the three polarizations of an external massive vector boson with rhe momentum `k` and mass $k^2$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Polariazation](Polariazation), [Uncontract](Uncontract).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*The standard formula for massless vector bosons is valid for all types of the corresponding particles, including gluons.*)


FCClearScalarProducts[]
SP[p]=0;
Pair[LorentzIndex[\[Mu]],Momentum[Polarization[p,-I]]] Pair[LorentzIndex[\[Nu]],Momentum[Polarization[p,I]]]


DoPolarizationSums[%,p,n]


(* ::Text:: *)
(*In QED the gauge invariance ensures the cancellation of the unphysical polarizations so that for photons one can also employ the simpler replacement with the metric tensor.*)


FCClearScalarProducts[]
SP[p]=0;
Pair[LorentzIndex[\[Mu]],Momentum[Polarization[p,-I]]] Pair[LorentzIndex[\[Nu]],Momentum[Polarization[p,I]]]


DoPolarizationSums[%,p,0]


(* ::Text:: *)
(*You can also use this trick in QCD, provided that the unphysical degrees of freedom are subtracted using ghosts at a later stage. Notice that in this case you should not make the polarization vectors transverse using the `Transversality` option. Furthermore, the averaging over the polarizations of the initial gluons must be done on the physical amplitude squared, i.e. after the ghost contributions have been subtracted.*)


(* ::Text:: *)
(*Massive vector bosons (e.g. W or Z) have 3 degrees of freedom and require no auxiliary vector.*)


FCClearScalarProducts[]
SP[p]=m^2;
Pair[LorentzIndex[\[Mu]],Momentum[Polarization[p,-I]]] Pair[LorentzIndex[\[Nu]],Momentum[Polarization[p,I]]]


DoPolarizationSums[%,p]


(* ::Text:: *)
(*A more realistic example of summing over the polarizations of the photons in $e^+e^ \to  \gamma \gamma$*)


ClearAll[s,t,u];
FCClearScalarProducts[];
SP[k1]=0;
SP[k2]=0;
(-((Spinor[Momentum[p1],0,1] . GS[Polarization[k1,I,Transversality->True]] . GS[k2-p2] . GS[Polarization[k2,I,Transversality->True]] . Spinor[-Momentum[p2],0,1]*SMP["e"]^2)/t)-(Spinor[Momentum[p1],0,1] . GS[Polarization[k2,I,Transversality->True]] . GS[k1-p2] . GS[Polarization[k1,I,Transversality->True]] . Spinor[-Momentum[p2],0,1]*SMP["e"]^2)/u)*(-((Spinor[-Momentum[p2],0,1] . GS[Polarization[k1,-I,Transversality->True]] . GS[k1-p2] . GS[Polarization[k2,-I,Transversality->True]] . Spinor[Momentum[p1],0,1]*SMP["e"]^2)/u)-(Spinor[-Momentum[p2],0,1] . GS[Polarization[k2,-I,Transversality->True]] . GS[k2-p2] . GS[Polarization[k1,-I,Transversality->True]] . Spinor[Momentum[p1],0,1]*SMP["e"]^2)/t)


%//DoPolarizationSums[#,k1,0]&//DoPolarizationSums[#,k2,0]&


(* ::Text:: *)
(*This is a small piece of the matrix element squared for $g g to  Q \bar{Q}$. The proper summation over the polarizations of the gluons requires a choice of two auxiliary vectors (unless we subtract the unphysical contributions using ghosts). It is customary to take the 4-momentum of another gluon as the auxiliary vector in the summation formula.*)


(* ::Text:: *)
(*The option `ExtraFactor}` is used to average over the polarizations of the initial gluons.*)


ClearAll[s,t,u];
FCClearScalarProducts[];
SP[p1]=0;
SP[p2]=0;


1/(s^2 SUNN (1-SUNN^2) u^2) 2 SMP["g_s"]^4 SP[k1,Polarization[p2,-I,Transversality->True]] SP[k1,Polarization[p2,I,Transversality->True]] (2 s^2 SP[k1,Polarization[p1,I,Transversality->True]] SP[k2,Polarization[p1,-I,Transversality->True]]+2 s SUNN^2 t SP[k1,Polarization[p1,I,Transversality->True]] SP[k2,Polarization[p1,-I,Transversality->True]]+s SUNN^2 u SP[k1,Polarization[p1,I,Transversality->True]] SP[k2,Polarization[p1,-I,Transversality->True]]+2 s^2 SP[k1,Polarization[p1,-I,Transversality->True]] SP[k2,Polarization[p1,I,Transversality->True]]+2 s SUNN^2 t SP[k1,Polarization[p1,-I,Transversality->True]] SP[k2,Polarization[p1,I,Transversality->True]]+s SUNN^2 u SP[k1,Polarization[p1,-I,Transversality->True]] SP[k2,Polarization[p1,I,Transversality->True]]+2 SUNN^2 u^2 SP[k2,Polarization[p1,-I,Transversality->True]] SP[k2,Polarization[p1,I,Transversality->True]])


%//DoPolarizationSums[#,p1,p2,ExtraFactor -> 1/2]&//DoPolarizationSums[#,p2,p1,ExtraFactor -> 1/2]&//Simplify
