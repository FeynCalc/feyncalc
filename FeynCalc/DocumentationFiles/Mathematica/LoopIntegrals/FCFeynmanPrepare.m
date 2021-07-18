(* ::Package:: *)

 


(* ::Section:: *)
(* FCFeynmanPrepare *)


(* ::Text:: *)
(*`FCFeynmanPrepare[int, {q1, q2, ...}]` is an auxiliary function that returns all necessary building for writing down a Feynman parametrization of the given tensor or scalar multi-loop integral.The integral int can be Lorentzian or Cartesian.*)


(* ::Text:: *)
(*The output of the function is a list given by `{U,F, pows, M, Q, J, N, r}`, where `U` and `F` are the Symanzik polynomials, with $U = det M$, while `pows` contains the powers of the occurring propagators. The vector `Q` and the function `J` are the usual quantities appearing in the definition of the F`` polynomial.*)


(* ::Text:: *)
(*If the integral has free indices, then `N` encodes its tensor structure, while `r` gives its tensor rank. For scalar integrals `N` is always `1` and r is `0`. In `N` the `F`-polynomial is not substituted but left as `FCGV["F"]`.*)


(* ::Text:: *)
(*To ensure a certain correspondence between propagators and Feynman parameters, it is also possible to enter the integral as a list of propagators, e.g. `FCFeynmanPrepare[{FAD[{q,m1}],FAD[{q-p,m2}],SPD[p,q]},{q}]`. In this case the tensor part of the integral should be the very last element of the list.*)


(* ::Text:: *)
(*The definitions of `M`, `Q`, `J` and `N` follow from Eq. 4.17 in the [PhD Thesis of Stefan Jahn](http://mediatum.ub.tum.de/?id=1524691) and [arXiv:1010.1667](https://arxiv.org/abs/1010.1667).The algorithm for deriving the UF-parametrization of a loop integral was adopted from the UF generator available in multiple codes of Alexander Smirnov, such as FIESTA ([arXiv:1511.03614](https://arxiv.org/abs/1511.03614)) and FIRE ([arXiv:1901.07808](https://arxiv.org/abs/1901.07808)). The code UF.m is also mentioned in the book "Analytic Tools for Feynman Integrals" by Vladimir Smirnov, Chapter 2.3.*)


(* ::Subsection:: *)
(* See also *)


(* ::Text:: *)
(*FCFeynmanParametrize, FCFeynmanProjectivize.*)


(* ::Subsection:: *)
(* Examples *)


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
(*`FCFeynmanPrepare` also works with `FCTopology` objects*)


FCFeynmanPrepare[FCTopology[fctopology1,{SFAD[{{p1,0},{0,1},1}],SFAD[{{p2,0},{0,1},1}],
SFAD[{{p3,0},{0,1},1}],SFAD[{{p1+p2+p3-Q,0},{0,1},1}],SFAD[{{p2+p3,0},{0,1},1}],
SFAD[{{p2-Q,0},{0,1},1}],SFAD[{{p1-Q,0},{0,1},1}],SFAD[{{p2+p3-Q,0},{0,1},1}],
SFAD[{{p1+p3-Q,0},{0,1},1}]}],{p1,p2,p3},Names->x,FCE->True]
