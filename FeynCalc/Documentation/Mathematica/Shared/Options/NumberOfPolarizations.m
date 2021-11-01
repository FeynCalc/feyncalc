(* ::Package:: *)

 


(* ::Section:: *)
(*NumberOfPolarizations*)


(* ::Text:: *)
(*`NumberOfPolarizations` is an option for `DoPolarizationSums`. It specifies the number of polarizations to sum over in the expression.*)
(*This is relevant only for expressions that contain terms free of polarization vectors. This may occur e.g. if the scalar products involving*)
(*polarization vectors have already been assigned some particular values. In this case the corresponding terms will be multiplied by the*)
(*corresponding number of polarizations.*)
(**)
(*The default value is `Automatic` which means that the function will attempt to recognize the correct value automatically by*)
(*extracting the dimension `dim` of the polarization vectors and putting `(dim-2)` for massless and `(dim-1)` for massive vector bosons.*)
(*Notice that if the input expression is free of polarization vectors, the setting `Automatic` will fail, and the user must specify the correct*)
(*dimension by hand.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [DoPolarizationSums](DoPolarizationSums.md).*)


(* ::Subsection:: *)
(*Examples*)


PolarizationVector[p,mu]ComplexConjugate[PolarizationVector[p,mu]]


(* ::Text:: *)
(*Here the setting Automatic is sufficient.*)


FCClearScalarProducts[];
ScalarProduct[p,p]=0;
PolarizationVector[p,mu]ComplexConjugate[PolarizationVector[p,mu]]+xyz
DoPolarizationSums[%,p,n]


(* ::Text:: *)
(*Here it is not*)


DoPolarizationSums[xyz,p,n]


(* ::Text:: *)
(*Setting the number of polarizations by hand fixes the issue*)


DoPolarizationSums[xyz,p,n,NumberOfPolarizations->2]



