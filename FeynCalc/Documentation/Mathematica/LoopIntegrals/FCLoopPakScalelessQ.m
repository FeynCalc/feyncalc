(* ::Package:: *)

(* ::Section:: *)
(*FCLoopPakScalelessQ*)


(* ::Text:: *)
(*`FCLoopPakScalelessQ[poly, x]` checks whether the characteristic polynomial poly (in the $U \times xF$ form) with the Feynman parameters `x[1], x[2], ...` corresponds to a scaleless loop integral or loop integral topology. The polynomial does not need to be canonically ordered.*)


(* ::Text:: *)
(*The function uses the algorithm of Alexey Pak [arXiv:1111.0868](https://arxiv.org/abs/1111.0868). Cf. also the PhD thesis of Jens Hoff [10.5445/IR/1000047447](https://doi.org/10.5445/IR/1000047447) for the detailed description of a possible implementation. `FCLoopPakScalelessQ`  is a backend function used in `FCLoopScalelessQ`, `FCLoopFindSubtopologies` etc.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopToPakForm](FCLoopToPakForm.md), [FCLoopScalelessQ](FCLoopScalelessQ.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*A scaleless 2-loop tadpole is a clear case, since here the characteristic polynomial vanishes*)


int=FAD[p1,p2,p1-p2]
pf=FCLoopToPakForm[int,{p1,p2},Names->x,
CharacteristicPolynomial->Function[{u,f}, u f]][[2]][[1]]


FCLoopPakScalelessQ[pf,x]


(* ::Text:: *)
(*A somewhat less obvious (but still simple) case is this 1-loop eikonal integral.*)


int=SFAD[{{0,2 p . q},0},p]
pf=FCLoopToPakForm[int,{p},Names->x,
CharacteristicPolynomial->Function[{u,f}, u f]][[2]][[1]]


FCLoopPakScalelessQ[pf,x]


(* ::Text:: *)
(*Adding a mass term to the quadratic propagator makes this integral nonvanishing*)


int=SFAD[{{0,2 p . q},0},{p,m^2}]
pf=FCLoopToPakForm[int,{p},Names->x,
CharacteristicPolynomial->Function[{u,f}, u f]][[2]][[1]]


FCLoopPakScalelessQ[pf,x]


(* ::Text:: *)
(*Notice that `FCLoopPakScalelessQ` is more of an auxiliary function. The corresponding end-user function is called `FCLoopScalelessQ`*)


FCLoopScalelessQ[SFAD[{{0,2 p . q},0},p],{p}]
