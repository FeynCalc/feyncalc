(* ::Package:: *)

 


(* ::Section:: *)
(*FCRenameDummyIndices*)


(* ::Text:: *)
(*`FCRenameDummyIndices[expr]` identifies dummy indices and changes their names pairwise to random symbols. This can be useful if you have an expression that contains dummy indices and want to compute the square of it. For example, the square of `GA[a, l, a]` equals $16$. However, if you forget to rename the dummy indices and compute `GA[a, l, a, a, l, a]` instead of `GA[a, l, a, b, l, b]`, you will get $64$.*)


(* ::Text:: *)
(*Notice that this routine does not perform any canonicalization. Use `FCCanonicalizeDummyIndices` for  that.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ComplexConjugate](ComplexConjugate.md), [FCCanonicalizeDummyIndices](FCCanonicalizeDummyIndices.md).*)


(* ::Subsection:: *)
(*Examples*)


FVD[q,mu]FVD[p,mu]+FVD[q,nu]FVD[p,nu]+FVD[q,si]FVD[r,si]
FCRenameDummyIndices[%]//Factor2


Uncontract[SPD[q,p]^2,q,p,Pair->All]
FCRenameDummyIndices[%]


amp=-(Spinor[Momentum[k1],SMP["m_mu"],1] . GA[Lor1] . Spinor[-Momentum[k2],SMP["m_mu"],1]*Spinor[-Momentum[p2],SMP["m_e"],1] . GA[Lor1] . Spinor[Momentum[p1],SMP["m_e"],1]*FAD[k1+k2,Dimension->4]*SMP["e"]^2);
amp//FCRenameDummyIndices


CVD[p,i]CVD[q,i]+CVD[p,j]CVD[r,j]
%//FCRenameDummyIndices


SUNT[a,b,a]+SUNT[c,b,c]
%//FCRenameDummyIndices


DCHN[GA[mu],i,j]DCHN[GA[nu],j,k]
%//FCRenameDummyIndices


PCHN[CSI[a],i,j]PCHN[CSI[b],j,k]
%//FCRenameDummyIndices



