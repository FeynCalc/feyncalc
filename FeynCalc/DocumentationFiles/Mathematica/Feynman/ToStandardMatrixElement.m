(* ::Package:: *)

 


(* ::Section:: *)
(* ToStandardMatrixElement *)


(* ::Text:: *)
(*`ToStandardMatrixElement[exp]` wraps Dirac structures, color structures and polarization vectors with the head `StandardMatrixElement`.The idea of having standard matrix elements stems from A. Denner's "Techniques for the calculation of electroweak radiative corrections at the one-loop level and results for W-physics at LEP200", cf. arXiv:0709.1075.*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*DiracSubstitute5, DiracGamma, ToDiracGamma67, Spinor.*)


(* ::Subsection:: *)
(* Examples *)


Spinor[Momentum[k2,D],0,1] . GAD[\[Mu]] . Spinor[-Momentum[k1,D],0,1]*Spinor[-Momentum[ps,D],SMP["m_s"],1] . GAD[\[Mu]] . Spinor[Momentum[pd,D],SMP["m_d"],1]
ToStandardMatrixElement[%]
