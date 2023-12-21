(* ::Package:: *)

 


(* ::Section:: *)
(*NonCommHeadQ*)


(* ::Text:: *)
(*`NonCommHeadQ[exp]` yields `True` if the head of exp is a non-commutative object or `Dot`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [DeclareNonCommutative](DeclareNonCommutative.md), [UnDeclareNonCommutative](UnDeclareNonCommutative.md), [NonCommFreeQ](NonCommFreeQ.md), [NonCommQ](NonCommQ.md)*)


(* ::Subsection:: *)
(*Examples*)


NonCommHeadQ[GA[mu]]


NonCommHeadQ[GA[mu,nu,mu]]


NonCommHeadQ[FV[p,mu]]


NonCommHeadQ[FCI[SUNT[a]]]


NonCommHeadQ[FCI[SUNTF[a,i,j]]]
