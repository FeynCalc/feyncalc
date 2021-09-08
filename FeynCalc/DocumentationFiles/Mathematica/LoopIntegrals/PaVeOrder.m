(* ::Package:: *)

 


(* ::Section:: *)
(*PaVeOrder*)


(* ::Text:: *)
(*`PaVeOrder[expr]` orders the arguments of PaVe functions in expr in a standard way.*)


(* ::Text:: *)
(*`PaVeOrder[expr, PaVeOrderList -> { {..., s, u, ...}, {... m1^2, m2^2, ...}, ...}]` orders the arguments of `PaVe` functions in `expr` according to the specified ordering lists. The lists may contain only a subsequence of the kinematic variables.*)


(* ::Text:: *)
(*`PaVeOrder` has knows about symmetries in the arguments of PaVe functions with up to 6 legs.*)


(* ::Text:: *)
(*Available symmetry relations are saved here*)


FileBaseName/@FileNames["*.sym",FileNameJoin[{$FeynCalcDirectory, "Tables", "PaVeSymmetries"}]]


(* ::Text:: *)
(*For the time being, these tables contain relations for B-functions up to rank 10, C-functions up to rank 9, D-functions up to rank 8,*)
(*E-functions (5-point functions) up to rank 7 and F-functions (6-point functions) up to rank 4. If needed, relations for more legs*)
(*and higher tensor ranks can be calculated using FeynCalc and saved to PaVeSymmetries using template codes provided inside `*.sym` files.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PaVeReduce](PaVeReduce.md).*)


(* ::Subsection:: *)
(*Examples*)


ClearAll[t,s]


(* ::Text:: *)
(*Use PaVeOrder to change the ordering of arguments in a `D0` function*)


ex=D0[me2,me2,mw2,mw2,t,s,me2,0,me2,0]


PaVeOrder[ex,PaVeOrderList->{me2,me2,0,0}]


(* ::Text:: *)
(*Different orderings are possible*)


PaVeOrder[D0[me2,me2,mw2,mw2,t,s,me2,0,me2,0],PaVeOrderList->{0,0,me2,me2}]


(* ::Text:: *)
(*When applying the function to an amplitude containing multiple PaVe functions, one can specify a list of possible orderings*)


ex=D0[a,b,c,d,e,f,m12,m22,m32,m42]+D0[me2,me2,mw2,mw2,t,s,me2,0,me2,0]


PaVeOrder[ex,PaVeOrderList->{{me2,me2,0,0},{f,e}}]


(* ::Text:: *)
(*PaVeOrder can be useful to show that a particular linear combination of `PaVe` functions yields zero*)


diff=PaVe[0,0,{p14,p30,p24,p13,p20,p40,p34,p23,p12,p10},{m4,m3,m2,m1,m0},PaVeAutoOrder->False]-PaVe[0,0,{p10,p13,p12,p40,p30,p34,p20,p24,p14,p23},{m3,m0,m1,m4,m2},PaVeAutoOrder->False]


diff//PaVeOrder


(* ::Text:: *)
(*In most cases, such simplifications require not only 1-to-1 relations but also linear relations between PaVe functions. For example, here we have a 1-to-1 relation between $C_1$ and $C_2$*)


PaVe[2,{p10,p12,p20},{m1^2,m2^2,m3^2},PaVeAutoOrder->False]
PaVeOrder[%]


(* ::Text:: *)
(*It seems that `PaVeOrder` cannot rewrite $C_1$ in such a way, that the mass arguments appear as $m_2^2, m_1^2, m_3^2$*)


ex=PaVe[1,{p10,p12,p20},{m1^2,m2^2,m3^2},PaVeAutoOrder->False]


PaVeOrder[ex,PaVeOrderList->{m2,m1,m3}]


(* ::Text:: *)
(*In fact, such a rewriting is possible, but it involves a linear relation between multiple `PaVe` functions. To avoid an accidental*)
(*expression swell, by default `PaVeOrder` uses only 1-to-1 relations. Setting the option `Sum` to `True` allows the routine*)
(*to return linear relations too*)


PaVeOrder[ex,PaVeOrderList->{m2,m1,m3},Sum->True]


(* ::Text:: *)
(*When trying to minimize the number of `PaVe` functions in the expression, one often has to try different orderings first*)


diff=(C0[0,SP[p,p],SP[p,p],0,0,0]+2 PaVe[1,{0,SP[p,p],SP[p,p]},{0,0,0}]+PaVe[1,{SP[p,p],SP[p,p],0},{0,0,0}])


(* ::Text:: *)
(*This ordering doesn't look very helpful*)


PaVeOrder[diff,PaVeOrderList->{0,SP[p,p],SP[p,p]},Sum->True]
%//PaVeOrder


(* ::Text:: *)
(*But this one does the job*)


PaVeOrder[diff,PaVeOrderList->{SP[p,p],0,SP[p,p]},Sum->True]
%//PaVeOrder


(* ::Text:: *)
(*Here are few simpler cases*)


diff=PaVe[0,{0},{m2^2,m3^2}]+PaVe[1,{0},{m3^2,m2^2}]+PaVe[1,{0},{m2^2,m3^2}]


PaVeOrder[diff,PaVeOrderList->{m2,m3},Sum->True]


diff=PaVe[0,{0},{m2^2,m3^2}]+2 PaVe[1,{0},{m3^2,m2^2}]-PaVe[1,1,{0},{m2^2,m3^2}]+PaVe[1,1,{0},{m3^2,m2^2}]


PaVeOrder[diff,Sum->True,PaVeOrderList->{m3,m2}]


diff=PaVe[0,{0,0,0},{m2^2,m3^2,m4^2}]+2 PaVe[1,{0,0,0},{m2^2,m3^2,m4^2}]+2 PaVe[1,{0,0,0},{m3^2,m2^2,m4^2}]+
PaVe[1,1,{0,0,0},{m2^2,m3^2,m4^2}]-PaVe[1,1,{0,0,0},{m2^2,m4^2,m3^2}]+PaVe[1,1,{0,0,0},{m3^2,m2^2,m4^2}]+2 PaVe[1,2,{0,0,0},{m4^2,m2^2,m3^2}]


PaVeOrder[diff,Sum->True,PaVeOrderList->{m3,m2}]
