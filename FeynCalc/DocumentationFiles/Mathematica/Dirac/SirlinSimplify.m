 
(* ::Section:: *)
(*SirlinSimplify*)
(* ::Text:: *)
(*`SirlinSimplify[exp]` simplifies spinor chains that contain Dirac matrices using relations derived by Sirlin in Nuclear Physics B192 (1981) 93-99. Contrary to the original paper, the sign of the Levi-Civita tensor is choosen as $\varepsilon^{0123}=1$ which is the standard choice in FeynCalc.*)


(* ::Subsection:: *)
(*See also*)
(* ::Text:: *)
(*[DiracGamma](DiracGamma), [Spinor](Spinor), [SpinorChainTrick](SpinorChainTrick).*)



(* ::Subsection:: *)
(*Examples*)


SpinorUBar[p3,m3].GA[\[Mu],\[Rho],\[Nu],7].SpinorU[p1,m1] SpinorUBar[p4,m4].GA[\[Mu],\[Tau],\[Nu],7].SpinorU[p2,m2]
SirlinSimplify[%]
