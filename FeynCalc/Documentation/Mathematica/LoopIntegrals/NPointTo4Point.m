(* ::Package:: *)

 


(* ::Section:: *)
(*NPointTo4Point*)


(* ::Text:: *)
(*`NPointTo4Point[expr, q]` reduces scalar IR finite 5-point functions to scalar 4-point functions according to Eq. 4.52 in [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [PaVeReduce](PaVeReduce.md).*)


(* ::Subsection:: *)
(*Examples*)


FCClearScalarProducts[]
SPD[p1]=0;
SPD[p1,p4]=0;
SPD[p3,p4]=0;
SPD[p1,p2]=0;
SPD[p2,p4]=0;
int=FCI[FAD[{q,m0},{q+p1,0},{q+p2,0},{q+p3,0},{q+p4,0}]]


NPointTo4Point[int,q,FCE->True,FCVerbose->-1]
FCClearScalarProducts[]
