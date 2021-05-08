 
(* ::Section:: *)
(* FCFeynmanProjectivize *)
(* ::Text:: *)
(*FCFeynmanProjectivize[int] checks if the given Feynman integral is projective. If this is not the case, the integral will be projectivized..*)


(* ::Subsection:: *)
(* Examples *)
int=SFAD[{p3,mg^2}]SFAD[{p3-p1,mg^2}]SFAD[{{0,-2p1.q}}]

aux=FCFeynmanParametrize[SFAD[{p3,mg^2}]SFAD[{p3-p1,mg^2}]SFAD[{{0,-2p1.q}}],{p1,p3},Names->x,Indexed->True,FCReplaceD->{D->4-2ep},Simplify->True,
Assumptions->{mg>0,ep>0},FinalSubstitutions->{SPD[q]->qq,mg^2->mg2}]

FCFeynmanProjectivize[(x[2]*x[3])^(3*(-1+ep))*((x[2]+x[3])*(Pair[Momentum[q,D],Momentum[q,D]]*x[1]^2+mg2*x[2]*x[3]))^(1-2*ep),x,Assumptions->{qq>0,mg2>0,x[1]>=0,x[2]>=0}]
FCFeynmanProjectivize: The integral is not projective, trying to projectivize.
FCFeynmanProjectivize: Projective transformation successful, the integral is now projective.
