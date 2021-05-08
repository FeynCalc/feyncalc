 
(* ::Section:: *)
(* NPointTo4Point *)
(* ::Text:: *)
(*NPointTo4Point[expr, q] reduces scalar IR finite 5-point functions to scalar 4-point functions according to Eq. 4.52 in arXiv:0709.1075..*)


(* ::Subsection:: *)
(* Examples *)
FCClearScalarProducts[];
SPD[p1]=0;
FCI@FAD[{q,m0},{q+p1,0},{q+p2,0},{q+p3,0},{q+p4,0}]

NPointTo4Point[%,q,FCE->True,FCVerbose->-1]
NPointTo4Point: The reduction of the scalar 5-point function 1/(q^2-m0^2).((p1+q))^2.((p2+q))^2.((p3+q))^2.((p4+q))^2is valid only if all the IR divergences were explicitly regularized with fictitious masses!
