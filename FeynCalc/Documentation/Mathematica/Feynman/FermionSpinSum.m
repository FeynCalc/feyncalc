(* ::Package:: *)

 


(* ::Section:: *)
(*FermionSpinSum*)


(* ::Text:: *)
(*`FermionSpinSum[exp]` converts products of closed spinor chains in `exp` into Dirac traces. Both Dirac and Majorana particles are supported. It is understood, that `exp` represents a squared amplitude.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Spinor](Spinor.md), [ComplexConjugate](ComplexConjugate.md), [DiracTrace](DiracTrace.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*FeynCalc uses the customary relativistic normalization of the spinors.*)


SpinorUBar[Momentum[p],m] . SpinorU[Momentum[p],m]
FermionSpinSum[%]
DiracSimplify[%]


SpinorVBar[Momentum[p],m] . SpinorV[Momentum[p],m]
FermionSpinSum[%]
DiracSimplify[%]


amp=SpinorUBar[k1,m] . GS[p] . GA[5] . SpinorU[p1,m]
ampSq= amp ComplexConjugate[amp]


FermionSpinSum[ampSq]
DiracSimplify[%]



