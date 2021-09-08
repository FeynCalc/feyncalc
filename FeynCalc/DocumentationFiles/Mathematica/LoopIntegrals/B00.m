 
(* ::Section:: *)
(*B00*)
(* ::Text:: *)
(*`B00[pp, ma^2, mb^2]` is the Passarino-Veltman $B_{00}$-function, i.e., the coefficient function of the metric tensor. All arguments are scalars and have dimension mass squared.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [B0](B0.md), [B1](B1.md), [PaVe](PaVe.md).*)



(* ::Subsection:: *)
(*Examples*)


B00[SPD[p],m^2,M^2]


B00[SPD[p],m^2,m^2]


B00[SPD[p],m^2,M^2,BReduce->False]


B00[0,m^2,m^2]


B00[SmallVariable[M^2],m^2,m^2]
