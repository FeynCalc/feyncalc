(* ::Package:: *)

 


(* ::Section:: *)
(*FCRenameDummyIndices*)


(* ::Text:: *)
(*`FCRenameDummyIndices[expr]` identifies all dummy Lorentz and $SU(N)$ indices and changes their names pairwise to random symbols. This can be useful if you have an expression that contains dummy indices and want to compute the square of it. For example, the square of `GA[a, l, a]` equals $16$. However, if you forget to rename the dummy indices and compute `GA[a, l, a, a, l, a]` instead of `GA[a, l, a, b, l, b]`, you will get $64$.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ComplexConjugate](ComplexConjugate.md).*)


(* ::Subsection:: *)
(*Examples*)


FVD[q,mu]FVD[p,mu]+FVD[q,nu]FVD[p,nu]+FVD[q,si]FVD[r,si]
FCRenameDummyIndices[%]//Factor2


Uncontract[SPD[q,p]^2,q,p,Pair->All]
FCRenameDummyIndices[%]


amp=-(Spinor[Momentum[k1],SMP["m_mu"],1] . GA[Lor1] . Spinor[-Momentum[k2],SMP["m_mu"],1]*Spinor[-Momentum[p2],SMP["m_e"],1] . GA[Lor1] . Spinor[Momentum[p1],SMP["m_e"],1]*FAD[k1+k2,Dimension->4]*SMP["e"]^2);
amp//FCRenameDummyIndices
