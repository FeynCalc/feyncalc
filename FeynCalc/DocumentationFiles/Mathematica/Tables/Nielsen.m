(* ::Package:: *)

 


(* ::Section:: *)
(*Nielsen*)


(* ::Text:: *)
(*`Nielsen[i, j, x]` denotes Nielsen's polylogarithm.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [SimplifyPolyLog](SimplifyPolyLog.md).*)


(* ::Subsection:: *)
(*Examples*)


Nielsen[1,2,x]


(* ::Text:: *)
(*Numerical evaluation is done via*)
(*`N[Nielsen[n_,p_,x_]] := (-1)^(n+p-1)/(n-1)!/p! NIntegrate[Log[1-x t]^p Log[t]^(n-1)/t,{t,0,1}]`*)


N[Nielsen[1,2,.45]]


(* ::Text:: *)
(*Some special values are built in.*)


{Nielsen[1,2,0],Nielsen[1,2,-1],Nielsen[1,2,1/2],Nielsen[1,2,1]}


Nielsen[1,2,x,PolyLog->True]


Nielsen[1,3,x,PolyLog->True]


Nielsen[3,1,x,PolyLog->True]
