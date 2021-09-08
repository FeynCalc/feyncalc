(* ::Package:: *)

 


(* ::Section:: *)
(*SirlinSimplify*)


(* ::Text:: *)
(*`SirlinSimplify[exp]` simplifies spinor chains that contain Dirac matrices using relations derived by A. Sirlin in [Nuclear Physics B192 (1981) 93-99](https://doi.org/10.1016/0550-3213(81)90195-4). Contrary to the original paper, the sign of the Levi-Civita tensor is chosen as $\varepsilon^{0123}=1$ which is the standard choice in FeynCalc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DiracGamma](DiracGamma.md), [Spinor](Spinor.md), [SpinorChainTrick](SpinorChainTrick.md).*)


(* ::Subsection:: *)
(*Examples*)


SpinorUBar[p3,m3] . GA[\[Mu],\[Rho],\[Nu],7] . SpinorU[p1,m1] SpinorUBar[p4,m4] . GA[\[Mu],\[Tau],\[Nu],7] . SpinorU[p2,m2]
SirlinSimplify[%]
