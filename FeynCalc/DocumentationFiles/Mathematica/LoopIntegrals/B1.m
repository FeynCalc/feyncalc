 
(* ::Section:: *)
(* B1 *)
(* ::Text:: *)
(*B1[pp, ma^2, mb^2] the Passarino-Veltman $B_1$-function. All arguments are scalars and have dimension mass^2..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*B0, B00, B11, PaVe, PaVeReduce.*)



(* ::Subsection:: *)
(* Examples *)



B1[SPD[p],m^2,M^2]

B1[SPD[p],m^2,M^2,BReduce->False]

B1[SP[p],m^2,m^2]

B1[SPD[p],m^2,m^2,BReduce->False]

B1[m^2,m^2,0]

B1[m^2,m^2,0,BReduce->False]

B1[0,0,m^2]

B1[pp,SmallVariable[SMP["m_e"]^2],Subsuperscript[m, 2, 2]]
