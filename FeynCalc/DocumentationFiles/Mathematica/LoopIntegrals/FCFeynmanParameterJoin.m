(* ::Package:: *)

 


(* ::Section:: *)
(*FCFeynmanParameterJoin*)


(* ::Text:: *)
(*`FCFeynmanParameterJoin[{{{prop1,prop2,x},prop3,y},...}, {p1,p2,...}]` joins all propagators in `int` using Feynman parameters but does not integrate over the loop momenta $p_i$. The function returns `{fpInt,pref,vars}`, where `fpInt` is the piece of the integral that contains a single `GFAD`-type propagator and `pref` is the part containing the `res`. The introduced Feynman parameters are listed in vars. The overall Dirac delta is omitted.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md).*)


(* ::Subsection:: *)
(*Examples*)


testProps={FAD[{p1,m1}],FAD[{p2,m2}],FAD[{p3,m3}],FAD[{p4,m4}]}


(* ::Text:: *)
(*Let us first join two propagators with each other using Feynman parameters `x[i]`*)


FCFeynmanParameterJoin[{testProps[[1]],testProps[[2]],x},{p1,p2,p3,p4}]


(* ::Text:: *)
(*Now we can join the resulting propagator with another propagator by introducing another set of Feynman parameters `y[i]`*)


FCFeynmanParameterJoin[{{testProps[[1]],testProps[[2]],x},testProps[[3]],y},{p1,p2,p3,p4}]


(* ::Text:: *)
(*If needed, this procedure can be nested even further*)


FCFeynmanParameterJoin[{{{testProps[[1]],testProps[[2]],x},testProps[[3]],y},testProps[[4]],z},{p1,p2,p3,p4}]


(* ::Text:: *)
(*Notice that `FCFeynmanParametrize`knows how to deal with the output produced by `FCFeynmanParameterJoin`*)


intT=FCFeynmanParameterJoin[{{SFAD[{p1,mg^2}]SFAD[{p3-p1,mg^2}],1,x},SFAD[{{0,-2p1 . q}}]SFAD[{{0,-2p3 . q}}],y},{p1,p3}]


FCFeynmanParametrize[intT[[1]],intT[[2]],{p1,p3},Names->z,Indexed->True,FCReplaceD->{D->4-2ep},Simplify->True,
Assumptions->{mg>0,ep>0},FinalSubstitutions->{FCI@SPD[q]->qq,mg^2->mg2},Variables->intT[[3]]]
