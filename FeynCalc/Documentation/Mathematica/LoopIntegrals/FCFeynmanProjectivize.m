(* ::Package:: *)

 


(* ::Section:: *)
(*FCFeynmanProjectivize*)


(* ::Text:: *)
(*`FCFeynmanProjectivize[int]` checks if the given Feynman integral is a projective form. If this is not the case, the integral will be projectivized.*)


(* ::Text:: *)
(*Projectivity is a necessary condition for computing the integral with the aid of the Cheng-Wu theorem*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanPrepare](FCFeynmanPrepare.md).*)


(* ::Subsection:: *)
(*Examples*)


int=SFAD[{p3,mg^2}]SFAD[{p3-p1,mg^2}]SFAD[{{0,-2p1 . q}}]


fp=FCFeynmanParametrize[int,{p1,p3},Names->x,Indexed->True,FCReplaceD->{D->4-2ep},Simplify->True,
Assumptions->{mg>0,ep>0},FinalSubstitutions->{SPD[q]->qq,mg^2->mg2}]


FCFeynmanProjectivize[fp[[1]],x]


FCFeynmanProjectivize[(x[1] + x[2])^(-2 + 2*ep)/(mb2*(x[1]^2 + x[1]*x[2] + x[2]^2))^ep,x]


FCFeynmanProjectivize[x[1]^(x - 1) (x[2])^(y - 1), x, {}]



