 
(* ::Section:: *)
(* NPointTo4Point *)
(* ::Text:: *)
(*`NPointTo4Point[expr, q]` reduces scalar IR finite 5-point functions to scalar 4-point functions according to Eq. 4.52 in [arXiv:0709.1075](https://arxiv.org/abs/0709.1075).*)


(* ::Subsection:: *)
(* Examples *)


FCClearScalarProducts[]
SPD[p1]=0;
FCI@FAD[{q,m0},{q+p1,0},{q+p2,0},{q+p3,0},{q+p4,0}]
NPointTo4Point[%,q,FCE->True,FCVerbose->-1]
FCClearScalarProducts[]
