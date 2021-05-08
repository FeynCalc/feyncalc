 
(* ::Section:: *)
(* FCFeynmanParametrize *)
(* ::Text:: *)
(*FCFeynmanParametrize[int, {q1, q2, ...}] introduces Feynman parameters for the scalar multi-loop integral int. The function returns {fpInt,pref,vars}, where fpInt is the integrand without the prefactor, pref is the prefactor free of Feynman parameters and vars is the list of integration variables. The overall Dirac delta in the integrand is omitted unless the option DiracDelta is set to True..*)


(* ::Subsection:: *)
(* Examples *)
FCFeynmanParametrize[FAD[{q,m}]FAD[{q-p,m2}]FVD[q,mu]FVD[q,nu],{q},Names->x,Indexed->False,FCE->True]
