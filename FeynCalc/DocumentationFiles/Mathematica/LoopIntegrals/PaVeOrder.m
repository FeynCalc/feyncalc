 
(* ::Section:: *)
(* PaVeOrder *)
(* ::Text:: *)
(*PaVeOrder[expr] orders the arguments of all D0 in expr in a standard way. PaVeOrder[expr, PaVeOrderList -> { {..., s, u, ...}, {... $m_1{}^2$, $m_2{}^2$ ...}, ...}] orders the arguments of all D0 in expr according to the specified ordering lists. The lists may contain only a subsequence of the D0-variables..*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*PaVeReduce.*)



(* ::Subsection:: *)
(* Examples *)



ClearAll[t,s]
PaVeOrder[D0[me2,me2,mw2,mw2,t,s,me2,0,me2,0],PaVeOrderList->{me2,me2,0,0}]

PaVeOrder[D0[me2,me2,mw2,mw2,t,s,me2,0,me2,0],PaVeOrderList->{me2,me2,0,0}]

PaVeOrder[D0[a,b,c,d,e,f,m12,m22,m32,m42]+D0[me2,me2,mw2,mw2,t,s,me2,0,me2,0],PaVeOrderList->{{me2,me2,0,0},{f,e}}]
