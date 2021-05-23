(* ::Package:: *)

 


(* ::Section:: *)
(* PaVeOrder *)


(* ::Text:: *)
(*`PaVeOrder[expr]` orders the arguments of PaVe functions in expr in a standard way.*)


(* ::Text:: *)
(*`PaVeOrder[expr, PaVeOrderList -> { {..., s, u, ...}, {... m1^2, m2^2, ...}, ...}]` orders the arguments of `PaVe` functions in `expr` according to the specified ordering lists. The lists may contain only a subsequence of the kinematic variables.*)


(* ::Text:: *)
(*`PaVeOrder` has knows about symmetries in the arguments of PaVe functions with up to 6 legs.*)


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


diff=PaVe[0,0,{p14,p30,p24,p13,p20,p40,p34,p23,p12,p10},{m4,m3,m2,m1,m0},PaVeAutoOrder->False]-PaVe[0,0,{p10,p13,p12,p40,p30,p34,p20,p24,p14,p23},{m3,m0,m1,m4,m2},PaVeAutoOrder->False]
diff//PaVeOrder



