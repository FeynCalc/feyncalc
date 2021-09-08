(* ::Package:: *)

 


(* ::Section:: *)
(*Tdec*)


(* ::Text:: *)
(*`Tdec[{{qi, mu}, {qj, nu}, ...}, {p1, p2, ...}]` calculates the tensorial decomposition formulas for Lorentzian integrals. The more common ones are saved in `TIDL`.*)


(* ::Text:: *)
(*The automatic symmetrization of the tensor basis is done using Alexey Pak's algorithm described in [arXiv:1111.0868](https://arxiv.org/abs/1111.0868).*)


(* ::Text:: *)
(**)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [TID](TID.md), [TIDL](TIDL.md), [OneLoopSimplify](OneLoopSimplify.md).*)


(* ::Subsection:: *)
(*Examples*)


(* ::Text:: *)
(*Check that $\int d^D f(p,q) q^{\mu}= \frac{p^{\mu}}{p^2} \int d^D f(p,q) p \cdot q$*)


Tdec[{q,\[Mu]},{p}]
%[[2]]/.%[[1]]


(* ::Text:: *)
(*This calculates integral transformation for any $\int d^D q_1 d^D q_2 d^D q_3$ $f(p,q_1,q_2,q_3) q_1^{\mu} q_2^{\nu}q_3^{\rho}$.*)


Tdec[{{Subscript[q, 1],\[Mu]},{Subscript[q, 2],\[Nu]},{Subscript[q, 3],\[Rho]}},{p},List->False]
Contract[% FVD[p,\[Mu]]FVD[p,\[Nu]] FVD[p,\[Rho]]]//Factor



