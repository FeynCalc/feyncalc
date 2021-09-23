(* ::Package:: *)

 


(* ::Section:: *)
(*PaVeReduce*)


(* ::Text:: *)
(*`PaVeReduce[expr]` reduces all Passarino-Veltman integrals (i.e. all PaVe's) in expr down to scalar `A0`, `B0`, `C0` and `D0`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FRH](FRH.md), [PaVeOrder](PaVeOrder.md).*)


(* ::Subsection:: *)
(*Examples*)


PaVeReduce[PaVe[1,2,{s,m^2,m^2},{m^2,m^2,M^2}],IsolateNames->FF]
FRH[%]


(* ::Text:: *)
(*The reduction results can be saved to a Mathematica file*)


PaVeReduce[PaVe[2,{SmallVariable[me2],mw2,t},{SmallVariable[me2],0,mw2}],
WriteOutPaVe->"p"]
TableForm[ReadList["pPaVe1Cmw2tsmame2C0mw2smame2.s",String]]
DeleteFile/@FileNames["pPaVe1Cmw2tsmame2C0mw2smame2.s"];


(* ::Text:: *)
(*Fortran export is also available*)


se=SmallVariable[ME2];
d122=PaVeReduce[PaVe[1,2,2,{se,MW2,MW2,se,S,T},{0,se,0,se}],Mandelstam->{S,T,U,2 MW2},IsolateNames->F]//FRH
Write2["fctd122.for",d122res==d122,FormatType->FortranForm];
TableForm[ReadList["fctd122.for",String]]
DeleteFile/@FileNames["fctd122.for"];Clear[d122,se]; 



