(* ::Package:: *)

 


(* ::Section:: *)
(*TimedIntegrate*)


(* ::Text:: *)
(*`TimedIntegrate[exp, vars]` is like `Integrate`, but stops after the number of seconds specified by the option `Timing`. Options of `Integrate` can be given and are passed on.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md)*)



(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*This should reach to be done*)


TimedIntegrate[Log[x^5],{x,0,1},Timing->1]


(* ::Text:: *)
(*This shouldn't*)


TimedIntegrate[Log[Cos[x^5]],{x,0,1},Timing->10,Integrate->int]
