(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopSingularityStructure*)


(* ::Text:: *)
(*`FCLoopSingularityStructure[int, {q1, q2, ...}]` returns a list of expressions `{pref,U,F,gbF}` that are useful to analyze the singular behavior of the loop integral `int`.*)


(* ::Text:: *)
(*- `pref` is the $\varepsilon$-dependent prefactor of the Feynman parameter integral that can reveal an overall UV-singularity*)
(*- `U` and `F` denote the first and second Symanzik polynomials respectively*)
(*- `gbF` is the Groebner basis of ${F, \partial F / \partial x_i}$ with respect to the Feynman parameters*)


(* ::Text:: *)
(*The idea to search for solutions of Landau equations for the $F$-polynomial using Groebner bases was adopted from [1810.06270](https://arxiv.org/abs/1810.06270) and [2003.02451](https://arxiv.org/abs/2003.02451) by B. Ananthanarayan, Abhishek Pal, S. Ramanan Ratan Sarkar and Abhijit B. Das.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanPrepare](FCFeynmanPrepare.md), [FCFeynmanParametrize](FCFeynmanParametrize.md)*)


(* ::Subsection:: *)
(*Examples*)


(* ::Subsubsection:: *)
(*1-loop tadpole*)


out=FCLoopSingularityStructure[FAD[{q,m}],{q},Names->x]


(* ::Text:: *)
(*The integral has an apparent UV-singularity from the prefactor*)


Normal[Series[out[[1]],{Epsilon,0,-1}]]


(* ::Subsubsection:: *)
(*Massless 1-loop 2-point function*)


out=FCLoopSingularityStructure[FAD[q,q-p],{q},Names->x]


(* ::Text:: *)
(*The integral has an apparent UV-singularity from the prefactor*)


Normal[Series[out[[1]],{Epsilon,0,-1}]]


(* ::Text:: *)
(*but there is also an IR-divergence for $p^2 = 0$ (the trivial solution with all $x_i$ being 0 is not relevant here)*)


Reduce[Equal[#,0]&/@out[[4]]]


(* ::Subsubsection:: *)
(*1-loop massless box*)


out=FCLoopSingularityStructure[FAD[p,p+q1,p+q1+q2,p+q1+q2+q3],{p},Names->x,FinalSubstitutions->{SPD[q1]->0,SPD[q2]->0,SPD[q3]->0}]


(* ::Text:: *)
(*As expected a 1-loop box has no overall UV-divergence*)


Normal[Series[out[[1]],{Epsilon,0,-1}]]


(* ::Text:: *)
(*The form of the U-polynomial readily suggests that there is no UV-subdivergence (again as expected)*)


Reduce[out[[2]]==0,{x[1],x[2],x[3],x[4]}]


(* ::Text:: *)
(*As far as the IR-divergences are concerned, we find a rather nontrivial set of solutions satisfying Landau equations*)


Reduce[Equal[#,0]&/@out[[4]]]


(* ::Subsubsection:: *)
(*A 2-loop eikonal integral with massive and massless lines*)


out=FCLoopSingularityStructure[SFAD[{ p1,m^2}]SFAD[{ p3,m^2}]SFAD[{{0,2p1 . n}}]SFAD[{{0,2(p1+p3) . n}}],{p1,p3},Names->x,FinalSubstitutions->{SPD[n]->1,m->1}]


(* ::Text:: *)
(*The integral has no IR-divergence, the only solution to the Landau equations is a trivial one*)


Reduce[Equal[#,0]&/@out[[4]],Reals]


(* ::Text:: *)
(*Notice that the mass is acting as an IR regulator here. Setting it to 0 makes the IR pole resurface*)


out=FCLoopSingularityStructure[SFAD[{ p1,m^2}]SFAD[{ p3,m^2}]SFAD[{{0,2p1 . n}}]SFAD[{{0,2(p1+p3) . n}}],{p1,p3},Names->x,FinalSubstitutions->{SPD[n]->1,m->0}]


(* ::Text:: *)
(*and here is our nontrivial solution*)


Reduce[Equal[#,0]&/@out[[4]],Reals]
