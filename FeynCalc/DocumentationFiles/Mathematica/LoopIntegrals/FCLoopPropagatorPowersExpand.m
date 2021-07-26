(* ::Package:: *)

(* ::Section:: *)
(*FCLoopPropagatorPowersExpand*)


(* ::Text:: *)
(*`FCLoopPropagatorPowersExpand[exp]` rewrites propagators raised to integer powers as products.*)


(* ::Subsection:: *)
(*See also*)


(* ::Subsection:: *)
(*Examples*)


SFAD[{q,m,2}]
FCLoopPropagatorPowersExpand[%]
%//StandardForm


SFAD[{q,m,2},q+p]
FCLoopPropagatorPowersExpand[%]
%//StandardForm
