(* ::Package:: *)

 


(* ::Section:: *)
(*FCMatrixProduct*)


(* ::Text:: *)
(*`FCMatrixProduct[mat1, mat2, ...]` can be used to obtain products of matrices*)
(*with entries containing noncommutative symbols. Using the usual `Dot` on such matrices would otherwise destroy the original ordering.*)
(**)
(*The resulting expression can be then further simplified using `DotSimplify`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DataType](DataType.md), [DeclareNonCommutative](DeclareNonCommutative.md), [UnDeclareNonCommutative](UnDeclareNonCommutative.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Subsubsection:: *)
(*Generic matrices*)


(* ::Text:: *)
(*Consider two generic `2 \times 2`-matrices containing noncommutative heads*)


DeclareNonCommutative[opA,opB,opC,opD]


mat[1]={{opA[1],opB[1]},{opC[1],opD[1]}}
mat[2]={{opA[2],opB[2]},{opC[2],opD[2]}}


(* ::Text:: *)
(*Using the usual `Dot` product the elements of the resulting matrix are now multiplied with each other commutatively. Hence, the result is incorrect.*)


mat[1] . mat[2]


(* ::Text:: *)
(*With `FCMatrixProduct` the proper ordering is preserved*)


FCMatrixProduct[mat[1],mat[2]]


(* ::Text:: *)
(*We can also multiply more than two matrices at once*)


mat[3]={{opA[3],opB[3]},{opC[3],opD[3]}}


out=FCMatrixProduct[mat[1],mat[2],mat[3]]


(* ::Text:: *)
(*Now use `DotSimplify` to expand noncommutative products*)


DotSimplify[out]


(* ::Subsubsection:: *)
(*Dirac matrices in terms of Pauli matrices*)


(* ::Text:: *)
(*Let us define Dirac matrices in the Dirac basis in terms of Pauli matrices*)


gamma[0] = {{1, 0}, {0, -1}};
gamma[i_] := {{0, CSI[i]}, {-CSI[i], 0}};


(* ::Text:: *)
(*and express $\gamma^i \gamma^j \gamma^i$ as a $2 \times 2$-matrix*)


FCMatrixProduct[gamma[i], gamma[j], gamma[i]]
DotSimplify[%]



