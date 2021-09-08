 
(* ::Section:: *)
(*B1*)
(* ::Text:: *)
(*`B1[pp, ma^2, mb^2]` the Passarino-Veltman $B_1$-function. All arguments are scalars and have dimension mass squared.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [B0](B0.md), [B00](B00.md), [B11](B11.md), [PaVe](PaVe.md), [PaVeReduce](PaVeReduce.md).*)



(* ::Subsection:: *)
(*Examples*)


B1[SPD[p],m^2,M^2]


B1[SPD[p],m^2,M^2,BReduce->False]


B1[SP[p],m^2,m^2]


B1[SPD[p],m^2,m^2,BReduce->False]


B1[m^2,m^2,0]


B1[m^2,m^2,0,BReduce->False]


B1[0,0,m^2]


B1[pp,SmallVariable[SMP["m_e"]^2],Subsuperscript[m, 2, 2]]
