(* ::Package:: *)

 


(* ::Section:: *)
(*ComplexConjugate*)


(* ::Text:: *)
(*`ComplexConjugate[exp]` returns the complex conjugate of `exp`, where the input expression must be a proper matrix element. All Dirac matrices are assumed to be inside closed Dirac spinor chains. If this is not the case, the result will be inconsistent. Denominators may not contain explicit $i$'s.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCRenameDummyIndices](FCRenameDummyIndices.md), [FermionSpinSum](FermionSpinSum.md), [DiracGamma](DiracGamma.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*ComplexConjugate is meant to be applied to amplitudes, i.e. given a matrix element $\mathcal{M}$, it will return $\mathcal{M}^\ast$.*)


amp=(Spinor[Momentum[k1],SMP["m_e"],1] . GA[\[Mu]] . Spinor[Momentum[p2],SMP["m_e"],1]*
Spinor[Momentum[k2],SMP["m_e"],1] . GA[\[Nu]] . Spinor[Momentum[p1],SMP["m_e"],1]*
FAD[k1-p2,Dimension->4]*SMP["e"]^2-Spinor[Momentum[k1],SMP["m_e"],
1] . GA[\[Mu]] . Spinor[Momentum[p1],SMP["m_e"],1]*Spinor[Momentum[k2],
SMP["m_e"],1] . GA[\[Nu]] . Spinor[Momentum[p2],SMP["m_e"],1]*FAD[k2-p2,
Dimension->4]*SMP["e"]^2)


ComplexConjugate[amp]


(* ::Text:: *)
(*Although one can also apply the function to standalone Dirac matrices, it should be understood that the result is not equivalent to the complex conjugation of such matrices.*)


GA[\[Mu]]
ComplexConjugate[%]


GA[5]
ComplexConjugate[%]


(GS[Polarization[k1,-I,Transversality->True]] . (GS[k1-p2]+SMP["m_e"]) . 
GS[Polarization[k2,-I,Transversality->True]])
ComplexConjugate[%]


SUNTrace[SUNT[a,b,c]]
ComplexConjugate[%]


(* ::Text:: *)
(*Since FeynCalc 9.3 `ComplexConjugate` will automatically rename dummy indices.*)


PolarizationVector[p1,\[Mu]]PolarizationVector[p2,\[Nu]]MT[\[Mu],\[Nu]]
ComplexConjugate[%]


GA[\[Mu],\[Nu]]LC[\[Mu],\[Nu]][p1,p2]
ComplexConjugate[%]


(* ::Text:: *)
(*This behavior can be disabled by setting the option `FCRenameDummyIndices` to `False`.*)


ComplexConjugate[GA[\[Mu],\[Nu]]LC[\[Mu],\[Nu]][p1,p2],FCRenameDummyIndices->False]


(* ::Text:: *)
(*If particular variables must be replaced with their conjugate values, use the option `Conjugate`.*)


GA[\[Mu]] . (c1 GA[6]+c2 GA[7]) . GA[\[Nu]]
ComplexConjugate[%]


ComplexConjugate[GA[\[Mu]] . (c1 GA[6]+c2 GA[7]) . GA[\[Nu]],Conjugate->{c1,c2}]
%//StandardForm


(* ::Text:: *)
(*It may happen that one needs to deal with amplitudes with amputated spinors, i.e. with open Dirac or Pauli indices. If the amplitude contains only a single chain of Dirac/Pauli matrices, everything remains unambiguous and the missing spinors are understood*)


GA[\[Mu],\[Nu],\[Rho],5]CSI[i,j]
ComplexConjugate[%]


(* ::Text:: *)
(*However, when there are at least two spinor chains of the same type involved, such expressions do not make sense anymore. In these cases one should introduce explicit spinor indices to avoid ambiguities*)


DCHN[GA[\[Mu],\[Nu],\[Rho],5],i,j]DCHN[GA[\[Mu],\[Nu],\[Rho],5],k,l]
ComplexConjugate[%]


PCHN[CSI[i,j,k],a,b]PCHN[CSI[i,j,k],c,d]
ComplexConjugate[%]


(* ::Text:: *)
(*The function does not apply `Conjugate` to symbols that do not depend on `I` and are unrelated to Dirac/Pauli/Color matrices. One can specify symbols that need to be explicitly conjugated using the `Conjugate` option*)


cc SpinorU[p1] . GA[mu] . SpinorV[p2]
ComplexConjugate[%]


cc SpinorU[p1] . GA[mu] . SpinorV[p2]
ComplexConjugate[%,Conjugate->{cc}]



