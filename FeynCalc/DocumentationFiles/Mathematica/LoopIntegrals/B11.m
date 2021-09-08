 
(* ::Section:: *)
(*B11*)
(* ::Text:: *)
(*`B11[pp, ma^2, mb^2]` is the Passarino-Veltman $B_{11}$-function, i.e. the coefficient function of $p^{\mu } p^{\nu }$. All arguments are scalars and have dimension mass squared.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [B0](B0.md), [B00](B00.md), [B1](B1.md), [PaVe](PaVe.md).*)



(* ::Subsection:: *)
(*Examples*)


B11[SPD[p],m^2,M^2]


B11[SPD[p],m^2,M^2,BReduce->False]


B11[SPD[p],m^2,m^2]


B11[SPD[p],m^2,m^2,BReduce->False]


B11[0,m^2,m^2]


B11[0,m^2,m^2,BReduce->False]


B11[SmallVariable[M^2],m^2,m^2]


B11[SmallVariable[M^2],m^2,m^2,BReduce->False]
