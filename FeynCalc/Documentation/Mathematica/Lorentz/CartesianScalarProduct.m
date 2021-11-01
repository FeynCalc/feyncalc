(* ::Package:: *)

 


(* ::Section:: *)
(*CartesianScalarProduct*)


(* ::Text:: *)
(*`CartesianScalarProduct[p, q]`  is the input for the scalar product of two Cartesian vectors `p` and `q`.*)


(* ::Text:: *)
(*`CartesianScalarProduct[p]` is equivalent to `CartesianScalarProduct[p, p]`.*)


(* ::Text:: *)
(*Expansion of sums of momenta in `CartesianScalarProduct` is done with `ExpandScalarProduct`.*)


(* ::Text:: *)
(*Scalar products may be set, e.g. via `ScalarProduct[a, b] = m^2;` but `a` and `b` may not contain sums.*)


(* ::Text:: *)
(*`CartesianScalarProduct[a] ` corresponds to `CartesianScalarProduct[a,a] `*)


(* ::Text:: *)
(*Note that `ScalarProduct[a, b] = m^2` actually sets Cartesian scalar products in different dimensions specified by the value of the `SetDimensions` option.*)


(* ::Text:: *)
(*It is highly recommended to set `ScalarProduct`s before any calculation. This improves the performance of FeynCalc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [CSP](CSP.md), [CSPD](CSPD.md), [CSPE](CSPE.md).*)


(* ::Subsection:: *)
(*Examples*)


CartesianScalarProduct[p,q]


CartesianScalarProduct[p+q,-q]


CartesianScalarProduct[p,p]


CartesianScalarProduct[q]


CartesianScalarProduct[p,q]//StandardForm


CartesianScalarProduct[p,q,Dimension->D-1]//StandardForm


CartesianScalarProduct[Subscript[p, 1],Subscript[p, 2]] = s/2


ExpandScalarProduct[ CartesianScalarProduct[Subscript[p, 1]-q,Subscript[p, 2]-k]]


Calc[ CartesianScalarProduct[Subscript[p, 1]-q,Subscript[p, 2]-k]]


CartesianScalarProduct[q1]=qq;


CSP[q1]


FCClearScalarProducts[]
