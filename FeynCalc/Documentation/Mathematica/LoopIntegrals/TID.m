(* ::Package:: *)

 


(* ::Section:: *)
(*TID*)


(* ::Text:: *)
(*`TID[amp, q]` performs  tensor decomposition of 1-loop integrals with loop momentum `q`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [OneLoopSimplify](OneLoopSimplify.md), [TIDL](TIDL.md), [PaVeLimitTo4](PaVeLimitTo4.md).*)


(* ::Subsection:: *)
(*Examples*)


FCClearScalarProducts[];


int=FAD[{k,m}, k - Subscript[p, 1], k - Subscript[p, 2]]FVD[k,\[Mu]]//FCI


(* ::Text:: *)
(*By default, all tensor integrals are reduced to the Passarino-Veltman scalar integrals $A_0$, $B_0$, $C_0$, $D_0$ etc.*)


TID[int,k]


(* ::Text:: *)
(*Scalar integrals can be converted to the Passarino-Veltman notation via the option `ToPaVe`*)


TID[int,k,ToPaVe->True]


(* ::Text:: *)
(*We can force the reduction algorithm to use Passarino-Veltman coefficient functions via the option `UsePaVeBasis`*)


TID[int,k,UsePaVeBasis->True]


(* ::Text:: *)
(*Very often the integral can be simplified via partial fractioning even before performing the loop reduction. In this case the output will contain a mixture of `FAD` symbols and Passarino-Veltman functions*)


TID[SPD[p,q]FAD[q,{q-p,m}]FVD[q,mu],q,UsePaVeBasis->True]


(* ::Text:: *)
(*This can be avoided by setting both `UsePaVeBasis` and `ToPaVe` to `True`*)


TID[SPD[p,q]FAD[q,{q-p,m}]FVD[q,mu],q,UsePaVeBasis->True,ToPaVe->True]


(* ::Text:: *)
(*Alternatively, we may set `ToPaVe` to `Automatic` which will automatically invoke the `ToPaVe` function*)
(*if the final result contains even a single Passarino-Veltman function*)


TID[SPD[p,q]FAD[q,{q-p,m}]FVD[q,mu],q,ToPaVe->Automatic]


TID[SPD[p,q]FAD[q,{q-p,m}]FVD[q,mu],q,UsePaVeBasis->True,ToPaVe->Automatic]


(* ::Text:: *)
(*The basis of Passarino-Veltman coefficient functions is used automatically if there are zero Gram determinants*)


FCClearScalarProducts[];

SPD[Subscript[p, 1],Subscript[p, 1]]=M^2;

SPD[Subscript[p, 2],Subscript[p, 2]]=M^2;

SPD[Subscript[p, 1],Subscript[p, 2]]=M^2;

TID[FAD[{k,m}, k - Subscript[p, 1], k - Subscript[p, 2]]FVD[k,\[Mu]],k]


(* ::Text:: *)
(*A vanishing Gram determinant signals that the external momenta are linearly dependent on each other. This redundancy can be resolved by switching to a different basis. To that aim we need to run `FCLoopFindTensorBasis` to analyze the set of external momenta causing troubles*)


FCLoopFindTensorBasis[{-Subscript[p, 1], -Subscript[p, 2]},{},n]


(* ::Text:: *)
(*We see that $p_1$ and $p_2$ are proportional to each other, so that only one of these vectors is linearly independent. This also means that the scalar products involving loop momentum $p_1 \cdot k$ and $p_2 \cdot k$ are identical. Supplying this information to `TID` we can now achieve the desired reduction to scalars.*)


TID[FAD[{k,m}, k - Subscript[p, 1], k - Subscript[p, 2]]FVD[k,\[Mu]],k,
TensorReductionBasisChange->{{-Subscript[p, 1], -Subscript[p, 2]}->{-Subscript[p, 1]}},
FinalSubstitutions->{SPD[k,Subscript[p, 2]]->SPD[k,Subscript[p, 1]]}]


(* ::Text:: *)
(*Notice that the result contains a propagator squared. This can be reduced further using IBPs (e.g. by employing FIRE or KIRA via the FeynHelpers interface).*)


(* ::Text:: *)
(*For cases involving light-like external momenta we often need to introduce an auxiliary vector, since the available vector are not sufficient to form a basis*)


FCClearScalarProducts[];

SPD[p]=0;

TID[FAD[{k,m}, k - p]FVD[k,\[Mu]],k]


(* ::Text:: *)
(*Running `FCLoopFindTensorBasis` we get a suggestion to introduce an auxiliary vector $n$ to the basis. The scalar products of this vector with other external momenta must be nonvanishing, but we are free to make the vector itself light-like (for simplicity)*)


FCLoopFindTensorBasis[{-p},{},n]


SPD[n]=0;

TID[FAD[{k,m}, k - p]FVD[k,\[Mu]],k,TensorReductionBasisChange->{{-p}->{-p,n}},AuxiliaryMomenta->{n}]


(* ::Text:: *)
(*Unfortunately, in this case `TID` alone cannot eliminate the scalar products of $n$ with the loop momentum in the numerator. For that we need to use IBPs. Still, it manages to reduce the tensor integral to scalars, even though at this stage not all of them can be mapped to scalar PaVe functions. *)


(* ::Text:: *)
(*The dependence on the auxiliary vector $n$ must cancel in the final result for this integral, as the auxiliary vector is unphysical and the original integral does not depend on it. To arrive at these cancellations for more complicated tensor integral it might be necessary to exploit the relations between the physical vectors as given by `FCLoopFindTensorBasis` and contract those with $n$.*)


(* ::Text:: *)
(*In FeynCalc, Passarino-Veltman coefficient functions are defined in the same way as in LoopTools. If one wants to use a different definition, it is useful to activate the option GenPaVe*)


FCClearScalarProducts[];

SPD[Subscript[p, 1],Subscript[p, 1]]=0;

SPD[Subscript[p, 2],Subscript[p, 2]]=0;

SPD[Subscript[p, 1],Subscript[p, 2]]=0;

TID[FAD[{k,m}, k - Subscript[p, 1], k - Subscript[p, 2]]FVD[k,\[Mu]]//FCI,k,GenPaVe->True]

FCClearScalarProducts[];


(* ::Text:: *)
(*To simplify manifestly IR-finite 1-loop results written in terms of Passarino-Veltman functions, we may employ the option `PaVeLimitTo4` (must be used together with `ToPaVe`). The result is valid up to 0th order in `Epsilon`, i.e. sufficient for 1-loop calculations.*)


FCClearScalarProducts[];

int=(D-1)(D-2)/(D-3)FVD[p,mu]FVD[p,nu]FAD[p,p-q]


TID[int,p,ToPaVe->True]


TID[int,p,ToPaVe->True,PaVeLimitTo4->True]


(* ::Text:: *)
(*Sometimes one would like to have external momenta multiplied by symbolic parameters in the propagators. In this case one should first declare the corresponding variables to be of `FCVariable` type*)


DataType[a, FCVariable] = True;
DataType[b, FCVariable] = True;


ExpandScalarProduct[SP[P,Q]/.P->a P1 +b P2]

StandardForm[%]



