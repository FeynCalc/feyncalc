(* ::Package:: *)

 


(* ::Section:: *)
(*FCFeynmanParametrize*)


(* ::Text:: *)
(*`FCFeynmanParametrize[int, {q1, q2, ...}]` introduces Feynman parameters for the multi-loop integral int.*)


(* ::Text:: *)
(*The function returns `{fpInt,pref,vars}`,  where `fpInt` is the integrand in Feynman parameters, `pref` is the prefactor free of Feynman parameters and `vars` is the list of integration variables.*)


(* ::Text:: *)
(*If the chosen parametrization contains a Dirac delta multiplying the integrand, it will be omitted unless the option `DiracDelta` is set to True.*)


(* ::Text:: *)
(*By default `FCFeynmanParametrize` uses normalization that is common in multi-loop calculations, i.e. $\frac{1}{i \pi^{D/2}}$ or $\frac{1}{\pi^{D/2}}$ per loop for Minkowski or Euclidean/Cartesian integrals respectively.*)


(* ::Text:: *)
(*If you want to have the standard $\frac{1}{(2 \pi)^D}$ normalization or yet another value, please set the option `FeynmanIntegralPrefactor` accordingly. Following values are available*)


(* ::Text:: *)
(*- "MultiLoop1" - default value explained above*)
(*- "MultiLoop2" - like the default value but with an extra $e^{\gamma_E \frac{4-D}{2}}$ per loop*)
(*- "Textbook" - $\frac{1}{(2 \pi)^D}$ per loop*)
(*- "Unity" - no extra prefactor multiplying the integral measure*)


(* ::Text:: *)
(*The calculation of $D$-dimensional Minkowski integrals and $D-1$-dimensional Cartesian integrals is straightforward.*)


(* ::Text:: *)
(*To calculate a $D$-dimensional Euclidean integral (i.e. an integral defined with the Euclidean*)
(*metric signature $(1,1,1,1)$ you need to write it in terms of `FVD`, `SPD`, `FAD`, `SFAD` etc. and set the option `"Euclidean"` to `True`.*)


(* ::Text:: *)
(*The function can derive different representations of a loop integral. The choice of the representation is controlled by the option `Method`. Following representations are available*)


(* ::Text:: *)
(*- "Feynman" - the standard Feynman representation (default value). Both tensor integrals and integrals with scalar products in the numerator are supported.*)
(*- "Lee-Pomeransky" - this representation was first introduced in [1308.6676](https://arxiv.org/abs/1308.6676) by Roman Lee and Andrei Pomeransky. Currently, only scalar integrals without numerators are supported.*)


(* ::Text:: *)
(*`FCFeynmanParametrize` can also be employed in conjunction with `FCFeynmanParameterJoin`, where one first joins suitable propagators using auxiliary Feynman*)
(*parameters and then finally integrates out loop momenta.*)


(* ::Text:: *)
(*For a proper analysis of a loop integral one usually needs the `U` and `F` polynomials separately. Since internally `FCFeynmanParametrize` uses `FCFeynmanPrepare`, the information available from the latter is also accessible to `FCFeynmanParametrize`.*)


(* ::Text:: *)
(*By setting the option `FCFeynmanPrepare` to `True`, the output of `FCFeynmanPrepare` will be added the the output of `FCFeynmanParametrize` as the 4th list element.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanPrepare](FCFeynmanPrepare.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md), [FCFeynmanParameterJoin](FCFeynmanParameterJoin.md), [SplitSymbolicPowers](SplitSymbolicPowers.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Subsubsection:: *)
(*Feynman representation*)


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


(* ::Text:: *)
(*1-loop massless box*)


FAD[p,p+q1,p+q1+q2,p+q1+q2+q3]
FCFeynmanParametrize[%,{p},Names->x,FCReplaceD->{D->4-2Epsilon}]


(* ::Text:: *)
(*3-loop self-energy with two massive lines*)


SFAD[{{p1,0},{m^2,1},1},{{p2,0},{0,1},1},{{p3,0},{0,1},1},
{{p2+p3,0},{0,1},1},{{p1-Q,0},{m^2,1},1},{{p2-Q,0},{0,1},1},
{{p2+p3-Q,0},{0,1},1},{{p1+p2+p3-Q,0},{0,1},1}]
FCFeynmanParametrize[%,{p1,p2,p3},Names->x,FCReplaceD->{D->4-2Epsilon}]


(* ::Text:: *)
(*An example of using `FCFeynmanParametrize` together with `FCFeynmanParameterJoin`*)


props={SFAD[{p1,m^2}],SFAD[{p3,m^2}],SFAD[{{0,2p1 . n}}],SFAD[{{0,2(p1+p3) . n}}]}


intT=FCFeynmanParameterJoin[{{props[[1]]props[[2]],1,x},
props[[3]]props[[4]],y},{p1,p3}]


(* ::Text:: *)
(*Here the Feynman parameter variables $x_i$ and $y_i$ are independent from each other, i.e. we have $\delta(1-x_1-x_2-x_3) \times \delta(1-y_1-y_2-y_3)$.*)
(*This gives us much more freedom when exploiting the Cheng-Wu theorem.*)


FCFeynmanParametrize[intT[[1]],intT[[2]],{p1,p3},Indexed->True,FCReplaceD->{D->4-2ep},
FinalSubstitutions->{SPD[n]->1,m->1},Variables->intT[[3]]]


(* ::Text:: *)
(*In the case that we need `U` and `F` polynomials in addition to the normal output (e.g. for HyperInt)*)


(SFAD[{{0, 2*k1 . n}}]*SFAD[{{0, 2*k2 . n}}]*SFAD[{k1, m^2}]*
SFAD[{k2, m^2}]*SFAD[{k1 - k2, m^2}])
out=FCFeynmanParametrize[%,{k1,k2},Names->x,FCReplaceD->{D->4-2Epsilon}, 
FCFeynmanPrepare->True]


(* ::Text:: *)
(*From this output we can easily extract the integrand, its $x_i$-independent prefactor and the two Symanzik polynomials*)


{integrand,pref} = out[[1;;2]]
{uPoly,fPoly}=out[[4]][[1;;2]]


(* ::Text:: *)
(*Symbolic propagator powers are fully supported*)


SFAD[{I k,0,-1/2+ep},{I(k+p),0,1},EtaSign->-1]
v1=FCFeynmanParametrize[%,{k},Names->x,FCReplaceD->{D->4-2ep},FinalSubstitutions->{SPD[p]->1}]


(* ::Text:: *)
(*An alternative representation for symbolic powers can be obtained using the option `SplitSymbolicPowers`*)


SFAD[{I k,0,-1/2+ep},{I(k+p),0,1},EtaSign->-1]
v2=FCFeynmanParametrize[%,{k},Names->x,FCReplaceD->{D->4-2ep},FinalSubstitutions->{SPD[p]->1},SplitSymbolicPowers->True]


(* ::Text:: *)
(*Even though the parametric integrals evaluate to different values, the product of the integral and its prefactor remains the same*)


Integrate[Normal[Series[v1[[1]]/.x[1]->1,{ep,0,0}]]/.x[1]->1,{x[2],0,Infinity}]
Normal@Series[v1[[2]] %,{ep,0,0}]


Integrate[Normal[Series[v2[[1]]/.x[1]->1,{ep,0,0}]]/.x[1]->1,{x[2],0,Infinity}]
Normal@Series[v2[[2]] %,{ep,0,0}]


(* ::Subsubsection:: *)
(*Lee-Pomeransky representation*)


(* ::Text:: *)
(*1-loop tadpole*)


FCFeynmanParametrize[FAD[{q,m}],{q},Names->x,Method->"Lee-Pomeransky"]


(* ::Text:: *)
(*Massless 1-loop 2-point function*)


FCFeynmanParametrize[FAD[q,q-p],{q},Names->x,Method->"Lee-Pomeransky"]


(* ::Text:: *)
(*2-loop self-energy with 3 massive lines and two eikonal propagators*)


FCFeynmanParametrize[{SFAD[{ p1,m^2}],SFAD[{ p3,m^2}],
SFAD[{(p3-p1),m^2}],SFAD[{{0,2p1 . n}}],SFAD[{{0,2p3 . n}}]},{p1,p3},
Names->x,Method->"Lee-Pomeransky",FCReplaceD->{D->4-2ep},
FinalSubstitutions->{SPD[n]->1,m->1}]
