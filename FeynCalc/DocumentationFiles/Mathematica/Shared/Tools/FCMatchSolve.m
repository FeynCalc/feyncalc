(* ::Package:: *)

 


(* ::Section:: *)
(*FCMatchSolve*)


(* ::Text:: *)
(*`FCMatchSolve[expr, {notvar1, notvar2, ...}]` assumes that `expr` is a sum that must vanish term-wise and converts it to a system of linear equations. The function automatically determines which variables to solve for, excluding `notvar1, notvar2, ...` from the list.*)


(* ::Text:: *)
(*FCMatchSolve can also handle overdetermined systems of equations. This function is useful e.g. for determining renormalization constants or matching coefficients, where looking at each term separately and determining the values of the constants/coefficients by hand is too tedious.*)


(* ::Text:: *)
(*The input (say a sum or a difference of amplitudes) should be prepared using `Collect2` by collecting w.r.t distinct objects, e.g. matrix elements or coupling constants so that each term must vanish separately.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [Collect2](Collect2.md), [Solve2](Solve2.md), [Solve3](Solve3.md).*)


(* ::Subsection:: *)
(*Examples*)


FCMatchSolve[-1/8*(tauPref*(-128+64*nc+160*nc^2-8*nc*zz14+4*nc*zz44-
16*evFlag[4,3,1]+8*nc*evFlag[4,3,1]-16*evFlag[5,3,1]+8*nc*evFlag[5,3,1]+
nc*evFlag[9,3,1]-2*evFlag[10,3,1]+nc^2*evFlag[10,3,1])*OP[Q])/nc+
(tauPref*(-96*nc-96*nc^2+4*nc*zz24-4*nc*zz44-16*evFlag[4,3,1]+
24*nc*evFlag[4,3,1]+16*nc^2*evFlag[4,3,1]+16*evFlag[5,3,1]-
8*nc*evFlag[5,3,1]-nc*evFlag[9,3,1]+nc*evFlag[9,4,1]+2*evFlag[10,3,1]-
nc^2*evFlag[10,3,1]-2*evFlag[10,4,1]+nc^2*evFlag[10,4,1])*OP[QS])/(4*nc),
{OP[_],nc,evFlag[__],tauPref}]
