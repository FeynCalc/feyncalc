(* ::Package:: *)

 


(* ::Section:: *)
(* FCFeynmanParametrize *)


(* ::Text:: *)
(*`FCFeynmanParametrize[int, {q1, q2, ...}]` introduces Feynman parameters for the scalar multi-loop integral int. The function returns `{fpInt,pref,vars}`, where fpInt is the integrand without the `prefactor`, `pref` is the prefactor free of Feynman parameters and `vars` is the list of integration variables. The overall Dirac delta in the integrand is omitted unless the option `DiracDelta` is set to True.*)


(* ::Text:: *)
(*By default FCFeynmanParametrize uses normalization that is common in multi-loop calculations. If you want to have the standard $\frac{1}{(2 \pi)^D}$ normalization or yet another value, please set the option `FeynmanIntegralPrefactor` accordingly.*)


(* ::Text:: *)
(*To calculate $D$-dimensional Euclidean integrals (as opposed to $D-1$-dimensional Cartesian or $D$-dimensional Minkowski integrals) written in terms of `FVD`, `SPD`, `FAD`, `SFAD` etc., you need to set the option `"Euclidean"` to `True`.*)


(* ::Subsection:: *)
(* Examples *)


(* ::Text:: *)
(*1-loop tadpole*)


FCFeynmanParametrize[FAD[{q,m}],{q},Names->x]


(* ::Text:: *)
(*Massless 1-loop 2-point function*)


FCFeynmanParametrize[FAD[q,q-p],{q},Names->x]


(* ::Text:: *)
(*With $p^2$ replaced by `pp` and `D` set to `4 - 2 Epsilon`*)


FCFeynmanParametrize[FAD[q,q-p],{q},Names->x,FinalSubstitutions->SPD[p]->pp,FCReplaceD->{D->4-2Epsilon}]


(* ::Text:: *)
(*Standard text-book prefactor of the loop integral measure*)


FCFeynmanParametrize[FAD[q,q-p],{q},Names->x,FinalSubstitutions->SPD[p]->pp,FCReplaceD->{D->4-2Epsilon},
FeynmanIntegralPrefactor->"Textbook"]


(* ::Text:: *)
(*Same integral but with the Euclidean metric signature*)


FCFeynmanParametrize[FAD[q,q-p],{q},Names->x,FinalSubstitutions->SPD[p]->pp,FCReplaceD->{D->4-2Epsilon},
FeynmanIntegralPrefactor->"Textbook","Euclidean"->True]


(* ::Text:: *)
(*A tensor integral*)


FCFeynmanParametrize[FAD[{q,m}]FAD[{q-p,m2}]FVD[q,mu]FVD[q,nu],{q},Names->x,FCE->True]


(* ::Text:: *)
(*1-loop master formulas for Minkowski integrals (cf. Eq. 9.49b in Sterman's An introduction to QFT)*)


SFAD[{{k,2p . k},M^2,s}]
FCFeynmanParametrize[%,{k},Names->x,FCE->True,FeynmanIntegralPrefactor->1,FCReplaceD->{D->n}]


FVD[k,\[Mu]]SFAD[{{k,2p . k},M^2,s}]
FCFeynmanParametrize[%,{k},Names->x,FCE->True,FeynmanIntegralPrefactor->1,FCReplaceD->{D->n}]


FVD[k,\[Mu]]FVD[k,\[Nu]]SFAD[{{k,2p . k},M^2,s}]
FCFeynmanParametrize[%,{k},Names->x,FCE->True,FeynmanIntegralPrefactor->1,FCReplaceD->{D->n}]


(* ::Text:: *)
(*1-loop master formulas for Euclidean integrals (cf. Eq. 9.49a in Sterman's An introduction to QFT)*)


SFAD[{{k,2p . k},-M^2,s}]
FCFeynmanParametrize[%,{k},Names->x,FCE->True,"Euclidean"->True,FeynmanIntegralPrefactor->I]


FVD[k,\[Mu]]SFAD[{{k,2p . k},-M^2,s}]
FCFeynmanParametrize[%,{k},Names->x,FCE->True,FeynmanIntegralPrefactor->I,FCReplaceD->{D->n},"Euclidean"->True]


FVD[k,\[Mu]]FVD[k,\[Nu]]SFAD[{{k,2p . k},-M^2,s}]
FCFeynmanParametrize[%,{k},Names->x,FCE->True,FeynmanIntegralPrefactor->I,FCReplaceD->{D->n},"Euclidean"->True]



