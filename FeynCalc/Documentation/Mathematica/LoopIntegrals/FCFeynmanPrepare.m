(* ::Package:: *)

(* ::Section:: *)
(*FCFeynmanPrepare*)


(* ::Text:: *)
(*`FCFeynmanPrepare[int, {q1, q2, ...}]` is an auxiliary function that returns all necessary building for writing down a Feynman parametrization of the given tensor or scalar multi-loop integral. The integral int can be Lorentzian or Cartesian.*)


(* ::Text:: *)
(*The output of the function is a list given by `{U,F, pows, M, Q, J, N, r}`, where `U` and `F` are the Symanzik polynomials, with $U = det M$, while `pows` contains the powers of the occurring propagators. The vector `Q` and the function `J` are the usual quantities appearing in the definition of the F`` polynomial.*)


(* ::Text:: *)
(*If the integral has free indices, then `N` encodes its tensor structure, while `r` gives its tensor rank. For scalar integrals `N` is always `1` and r is `0`. In `N` the `F`-polynomial is not substituted but left as `FCGV["F"]`.*)


(* ::Text:: *)
(*To ensure a certain correspondence between propagators and Feynman parameters, it is also possible to enter the integral as a list of propagators, e.g. `FCFeynmanPrepare[{FAD[{q,m1}],FAD[{q-p,m2}],SPD[p,q]},{q}]`. In this case the tensor part of the integral should be the very last element of the list.*)


(* ::Text:: *)
(*It is also possible to invoke the function as `FCFeynmanPrepare[GLI[...], FCTopology[...]]` or `FCFeynmanPrepare[FCTopology[...]]`. Notice that in this case the value of the option `FinalSubstitutions` is ignored, as replacement rules will be extracted directly from the definition of the topology.*)


(* ::Text:: *)
(*The definitions of `M`, `Q`, `J` and `N` follow from Eq. 4.17 in the [PhD Thesis of Stefan Jahn](http://mediatum.ub.tum.de/?id=1524691) and [arXiv:1010.1667](https://arxiv.org/abs/1010.1667).The algorithm for deriving the UF-parametrization of a loop integral was adopted from the UF generator available in multiple codes of Alexander Smirnov, such as FIESTA ([arXiv:1511.03614](https://arxiv.org/abs/1511.03614)) and FIRE ([arXiv:1901.07808](https://arxiv.org/abs/1901.07808)). The code UF.m is also mentioned in the book "Analytic Tools for Feynman Integrals" by Vladimir Smirnov, Chapter 2.3.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCFeynmanParametrize](FCFeynmanParametrize.md), [FCFeynmanProjectivize](FCFeynmanProjectivize.md), [FCLoopValidTopologyQ](FCLoopValidTopologyQ.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*One of the simplest examples is the 1-loop tadpole*)


FCFeynmanPrepare[FAD[{q,m1}],{q}]


(* ::Text:: *)
(*Use the option `Names` to have specific symbols denoting Feynman parameters*)


FCFeynmanPrepare[FAD[{q,m1}],{q},Names->x]


(* ::Text:: *)
(*It is also possible to obtain e.g. `x1, x2, x3, ...` instead of `x[1], x[2], x[3], ...`*)


FCFeynmanPrepare[FAD[{q,m1}],{q},Names->x,Indexed->False]


(* ::Text:: *)
(*To fix the correspondence between Feynman parameters and propagators, the latter should be entered as a list*)


FCFeynmanPrepare[{FAD[{q,m}],FAD[{q-p,m2}],FVD[q,\[Mu]]FVD[q,\[Nu]]FVD[q,\[Rho]]},{q},Names->x]


(* ::Text:: *)
(*Massless 2-loop self-energy*)


FCFeynmanPrepare[FAD[p1,p2,Q-p1-p2,Q-p1,Q-p2],{p1,p2},Names->x]


(* ::Text:: *)
(*Factorizing integrals  also work*)


FCFeynmanPrepare[FAD[{p1,m1},{p2,m2},Q-p1,Q-p2],{p1,p2},Names->x]


(* ::Text:: *)
(*Cartesian propagators are equally supported*)


FCFeynmanPrepare[CSPD[q,p]CFAD[{q,m},{q-p,m2}],{q},Names->x]


(* ::Text:: *)
(*`FCFeynmanPrepare` also works with `FCTopology` and `GLI` objects*)


topo1=FCTopology["prop2Lv1",{SFAD[{p1,m1^2}],SFAD[{p2,m2^2}],SFAD[p1-q],SFAD[p2-q],SFAD[{p1-p2,m3^2}]},{p1,p2},{Q},{},{}]
topo2=FCTopology["prop2Lv2",{SFAD[{p1,m1^2}],SFAD[{p2,m2^2}],SFAD[{p1-q,M^2}],SFAD[{p2-q,M^2}],SFAD[p1-p2]},{p1,p2},{Q},{},{}]


FCFeynmanPrepare[topo1,Names->x]


FCFeynmanPrepare[{topo1,topo2},Names->x]


FCFeynmanPrepare[{GLI["prop2Lv1",{1,1,1,1,0}],GLI["prop2Lv2",{1,1,0,0,1}]},{topo1,topo2},Names->x]


(* ::Text:: *)
(*`FCFeynmanPrepare` can also handle products of `GLI`s.  In this case it will automatically introduce dummy names for the loop momenta (the name generation is controlled by the `LoopMomentum` option).*)


topo=FCTopology[
prop2Ltopo13311,{SFAD[{{I*p1,0},{-m1^2,-1},1}],SFAD[{{I*(p1+q1),0},{-
m3^2,-1},1}],SFAD[{{I*p3,0},{-m3^2,-1},1}],SFAD[{{I*(p3+q1),0},{-m1^2,
-1},1}],SFAD[{{I*(p1-p3),0},{-m1^2,-1},1}]},{p1,p3},{q1},{SPD[q1,q1]->m1^2},{}]


FCFeynmanPrepare[GLI[prop2Ltopo13311,{1,0,0,0,0}]^2,topo,Names->x,FCE->True,
LoopMomenta->Function[{x,y},lmom[x,y]]]
