 
(* ::Section:: *)
(*B0*)
(* ::Text:: *)
(*`B0[pp, ma^2, mb^2]` is the Passarino-Veltman two-point integral $B_0$. All arguments are scalars and have dimension mass squared. If the option `BReduce` is set to `True`, certain `B0`'s are reduced to `A0`'s. Setting the option `B0Unique` to `True` simplifies `B0[a,0,a]` and `B0[0,0,a]`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [B1](B1.md), [B00](B00.md), [B11](B11.md), [PaVe](PaVe.md).*)


(* ::Subsection:: *)
(*Examples*)


B0[SP[p,p],m^2,m^2]


B0[0,0,m^2,B0Unique->True,B0Real->True]


B0[m^2,0,m^2,B0Unique->True,B0Real->True]


B0[0,m^2,m^2]
