(* ::Package:: *)

 


(* ::Section:: *)
(*FCMultiLoopTID*)


(* ::Text:: *)
(*`FCMultiLoopTID[amp, {q1, q2, ...}]` does a multi-loop tensor integral decomposition, transforming the Lorentz indices away from the loop momenta `q1, q2, ...` The decomposition is applied only to the loop integrals where loop momenta are contracted with Dirac matrices or epsilon tensors.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCLoopFindTensorBasis](FCLoopFindTensorBasis.md), [TID](TID.md).*)


(* ::Subsection:: *)
(*Examples*)


FCI[FVD[q1,\[Mu]] FVD[q2,\[Nu]] FAD[q1,q2,{q1-p1},{q2-p1},{q1-q2}]]

FCMultiLoopTID[%,{q1,q2}]


(* ::Text:: *)
(*In the case of vanishing Gram determinants one can apply the same procedure as in the case of TID or FCLoopTensorReduce: one uses `FCLoopFindTensorBasis` to find a linear independent basis of external momenta and then supplies this basis to the function.*)


FCClearScalarProducts[]
SPD[p1] = m1^2;
SPD[p2] = m2^2;
SPD[p1, p2] = m1 m2;


FCMultiLoopTID[FVD[q1, mu] FAD[{q1, m}, {q1 + p1}, {q1 + p2}], {q1}]


FCLoopFindTensorBasis[{p1,p2},{},n]


FCMultiLoopTID[FVD[q1, mu] FAD[{q1, m}, {q1 + p1}, {q1 + p2}], {q1},
TensorReductionBasisChange -> {{p1, p2} -> {p1}}]
