(* ::Package:: *)

 


(* ::Section:: *)
(*FCDiracIsolate*)


(* ::Text:: *)
(*`FCDiracIsolate[exp]` wraps chains of Dirac matrices into heads specified by the user.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*`FCDiracIsolate` provides an easy way to extract the Dirac structures present in the expression (e.g. an amplitude)*)


amp=(Spinor[Momentum[p2],SMP["m_u"],1] . (-I GA[\[Mu]] SMP["g_s"] SUNTF[{Glu2},
Col3,Col5]) . (GS[-k1+p2]+SMP["m_u"]) . (-I GA[\[Nu]] SMP["g_s"] SUNTF[{Glu4},
Col5,Col1]) . Spinor[Momentum[p1],SMP["m_u"],1] FAD[{k1-p2,
SMP["m_u"]},Dimension->4] FV[Polarization[k1,I],\[Mu]] FV[Polarization[k2,-I],
\[Nu]]+Spinor[Momentum[p2],SMP["m_u"],1] . (-I GA[\[Nu]] SMP["g_s"] SUNTF[{Glu4},
Col3,Col5]) . (GS[k2+p2]+SMP["m_u"]) . (-I GA[\[Mu]] SMP["g_s"] SUNTF[{Glu2},
Col5,Col1]) . Spinor[Momentum[p1],SMP["m_u"],1] FAD[{-k2-p2,SMP["m_u"]},
Dimension->4] FV[Polarization[k1,I],\[Mu]] FV[Polarization[k2,
-I],\[Nu]]-Spinor[Momentum[p2],SMP["m_u"],1] . (-I GA[Lor3] SMP["g_s"] SUNTF[{Glu5},
Col3,Col1]) . Spinor[Momentum[p1],SMP["m_u"],1] FAD[-k1+k2,
Dimension->4] FV[Polarization[k1,I],\[Mu]] FV[Polarization[k2,-I],
\[Nu]] MT[Lor3,Lor4] (FV[2 k1-k2,\[Nu]] MT[Lor4,\[Mu]]+FV[-k1+2 k2,\[Mu]] MT[Lor4,
\[Nu]]+FV[-k1-k2,Lor4] MT[\[Mu],\[Nu]]) SMP["g_s"] SUNF[Glu2,Glu4,Glu5])


ampIso=FCDiracIsolate[amp,Head->diracS]


(* ::Text:: *)
(*Now that all Dirac structures are wrapped into the head `diracS` it is easy to extract them to a separate list*)


Cases2[ampIso,diracS]


(* ::Text:: *)
(*This way we obtain a sorted list of all unique Dirac structures in `amp`.*)


ClearAll[amp,ampIso,diracS]
