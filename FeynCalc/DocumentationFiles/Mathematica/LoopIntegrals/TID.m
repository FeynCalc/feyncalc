 
(* ::Section:: *)
(* TID *)
(* ::Text:: *)
(*TID[amp, q] performs  tensor decomposition of 1-loop integrals with loop momentum q.*)


(* ::Subsection:: *)
(* See also *)
(* ::Text:: *)
(*OneLoopSimplify, TIDL.*)



(* ::Subsection:: *)
(* Examples *)



FCClearScalarProducts[];
int=FAD[{k,m}, k - Subscript[p, 1], k - Subscript[p, 2]]FVD[k,\[Mu]]//FCI


(* ::Text:: *)
(*By default, all the tensor integrals are reduced to the Passarino-Veltman scalar integrals $A_0$, $B_0$, $C_0$, $D_0$ etc. *)


TID[int,k]


(* ::Text:: *)
(*Scalar integrals can be converted to the Passarino-Veltman notation via the option ToPaVe*)


TID[int,k,ToPaVe->True]


(* ::Text:: *)
(*We can force the reduction algorithm to use Passarino-Veltman coefficient functions via the option UsePaVeBasis*)


TID[int,k,UsePaVeBasis->True]


(* ::Text:: *)
(*The basis of Passarino-Veltman coefficient functions is used automatically if there are zero Gram determinants*)


FCClearScalarProducts[];
SPD[Subscript[p, 1],Subscript[p, 1]]=0;
SPD[Subscript[p, 2],Subscript[p, 2]]=0;
SPD[Subscript[p, 1],Subscript[p, 2]]=0;
TID[int,k]


(* ::Text:: *)
(*In FeynCalc, Passarino-Veltman coefficient functions are defined in the same way as in LoopTools, which is a quite common convention. If one wants to use a different definition, it is useful to activate the option GenPaVe*)


TID[int,k,GenPaVe->True]


(* ::Text:: *)
(*To simplify manifestly IR finite 1-loop results written in terms of Passarino-Veltman functions, we may employ the option PaVeLimitTo4 (must be used together with ToPaVe).\[LineSeparator]The result is valid up to 0th order in Epsilon, i.e. sufficient for 1-loop calculations.*)


FCClearScalarProducts[];
int=(D-1)(D-2)/(D-3)FVD[p,mu]FVD[p,nu]FAD[p,p-q]

TID[int,p,ToPaVe->True]

TID[int,p,ToPaVe->True,PaVeLimitTo4->True]
