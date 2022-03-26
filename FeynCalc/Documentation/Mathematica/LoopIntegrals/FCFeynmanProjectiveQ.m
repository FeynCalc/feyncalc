(* ::Package:: *)

 


(* ::Section:: *)
(*FCFeynmanProjectiveQ*)


(* ::Text:: *)
(*`FCFeynmanProjectiveQ[int, x]` checks if the given Feynman parameter integral (without prefactors) depending on x[1], x[2], ...  is a projective form.*)


(* ::Text:: *)
(*It is similar to `FCFeynmanProjectivize` but unlike the former it simply returns `True` or `False` depending*)
(*on whether the integral is projective or not.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanPrepare](FCFeynmanPrepare.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md).*)


(* ::Subsection:: *)
(*Examples*)


int=SFAD[{p3,mg^2}]SFAD[{p3-p1,mg^2}]SFAD[{{0,-2p1 . q}}]


fp=FCFeynmanParametrize[int,{p1,p3},Names->x,Indexed->True,FCReplaceD->{D->4-2ep},Simplify->True,
Assumptions->{mg>0,ep>0},FinalSubstitutions->{SPD[q]->qq,mg^2->mg2}]


FCFeynmanProjectiveQ[fp[[1]],x]


FCFeynmanProjectiveQ[(x[1] + x[2])^(-2 + 2*ep)/(mb2*(x[1]^2 + x[1]*x[2] + x[2]^2))^ep,x]


(* ::Text:: *)
(*Feynman parametrization derived from propagator representation should be projective in most cases.*)
(*However, arbitrary Feynman parameter integral do not necessarily have this property.*)


FCFeynmanProjectiveQ[x[1]^(x - 1) (x[2])^(y - 1), x]
