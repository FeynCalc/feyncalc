 
(* ::Section:: *)
(* FCFeynmanParameterJoin *)
(* ::Text:: *)
(*`FCFeynmanParameterJoin[int]` joins all propagators in `int` using Feynman parameters but does not integrate over the loop momenta. The function returns `{fpInt,pref,vars}`, where `fpInt` is the piece of the integral that contains a single `GFAD`-type propagator and `pref` is the part containing the `res`. The introduced Feynman parameters are listed in vars. The overall Dirac delta is omitted.*)


(* ::Subsection:: *)
(* Examples *)


intT=FCFeynmanParameterJoin[{{SFAD[{p1,mg^2}]SFAD[{p3-p1,mg^2}],1,x},SFAD[{{0,-2p1.q}}]SFAD[{{0,-2p3.q}}],y},{p1,p3}]


FCFeynmanParametrize[intT[[1]],intT[[2]],{p1,p3},Names->z,Indexed->True,FCReplaceD->{D->4-2ep},Simplify->True,
Assumptions->{mg>0,ep>0},FinalSubstitutions->{FCI@SPD[q]->qq,mg^2->mg2},Variables->intT[[3]]]
