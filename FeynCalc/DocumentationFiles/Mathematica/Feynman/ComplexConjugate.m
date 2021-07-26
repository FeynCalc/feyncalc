(* ::Package:: *)

 


(* ::Section:: *)
(*ComplexConjugate*)


(* ::Text:: *)
(*`ComplexConjugate[exp]` returns the complex conjugate of `exp`, where the input expression must be a proper matrix element. All Dirac matrices are assumed to be inside closed Dirac spinor chains. If this is not the case, the result will be inconsistent. Denominators may not contain explicit $i$'s.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCRenameDummyIndices](FCRenameDummyIndices), [FermionSpinSum](FermionSpinSum), [DiracGamma](DiracGamma).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*ComplexConjugate is meant to be applied to amplitudes, i.e. given a matrix element $\mathcal{M}$, it will return $\mathcal{M}^\ast$.*)


Spinor[Momentum[k1],SMP["m_e"],1] . GA[\[Mu]] . Spinor[Momentum[p2],SMP["m_e"],1]*Spinor[Momentum[k2],SMP["m_e"],1] . GA[\[Nu]] . Spinor[Momentum[p1],SMP["m_e"],1]*FAD[k1-p2,Dimension->4]*SMP["e"]^2-Spinor[Momentum[k1],SMP["m_e"],1] . GA[\[Mu]] . Spinor[Momentum[p1],SMP["m_e"],1]*Spinor[Momentum[k2],SMP["m_e"],1] . GA[\[Nu]] . Spinor[Momentum[p2],SMP["m_e"],1]*FAD[k2-p2,Dimension->4]*SMP["e"]^2


(* ::Text:: *)
(*Although one can also apply the function to standalone Dirac matrices, it should be understood that the result is not equivalent to the complex conjugation of such matrices.*)


GA[\[Mu]]
ComplexConjugate[%]


GA[5]
ComplexConjugate[%]


GS[Polarization[k1,-I,Transversality->True]] . (GS[k1-p2]+SMP["m_e"]) . GS[Polarization[k2,-I,Transversality->True]]
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
