(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopCreatePartialFractioningRules*)


(* ::Text:: *)
(*`FCLoopCreatePartialFractioningRules[glis, topos] applies partial fraction decomposition to the given GLIs provided that the corresponding topologies contain linearly dependent propagators. The output is given as a list containing replacement rules and new topologies generated in the course of the decomposition.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [ApartFF](ApartFF.md), [FCTopology](FCTopology.md), [GLI](GLI.md).*)


(* ::Subsection:: *)
(*Examples*)


glis={
GLI["preTopoDia2", {1, 1, 0, 0, 1}], 
GLI["preTopoDia1", {1, 0, 1, 1, 1}]}


topos={FCTopology["preTopoDia1", {SFAD[{{k1, 0}, {0, 1}, 1}], SFAD[{{0, mqb*k1 . nb}, 
{0, 1}, 1}], SFAD[{{k1, 2*gkin*meta*u0b*k1 . n}, {0, 1}, 1}], 
SFAD[{{k1, 2*gkin*meta*k1 . n - meta*u0b*k1 . nb}, {2*gkin*meta^2*u0b, 1}, 1}], 
SFAD[{{k1, 2*gkin*meta*u0b*k1 . n - meta*u0b*k1 . nb}, {2*gkin*meta^2*u0b^2, 1}, 
1}]}, {k1}, {n, nb}, {Hold[SPD][n] -> 0, Hold[SPD][nb] -> 0, Hold[SPD][n, nb] -> 2}, 
{}], FCTopology["preTopoDia2", {SFAD[{{k1, 0}, {0, 1}, 1}], SFAD[{{0, -(mqb*k1 . nb)}, {0, 1}, 1}], SFAD[{{k1, meta*u0b*k1 . nb}, {0, 1}, 1}], SFAD[{{k1, -2*gkin*meta*k1 . n + meta*u0b*k1 . nb}, {2*gkin*meta^2*u0b, 1}, 1}], 
   SFAD[{{k1, -2*gkin*meta*u0b*k1 . n + meta*u0b*k1 . nb}, {2*gkin*meta^2*u0b^2, 1}, 
   1}]}, {k1}, {n, nb}, {Hold[SPD][n] -> 0, Hold[SPD][nb] -> 0, Hold[SPD][n, nb] -> 2},
{}]}


FCLoopCreatePartialFractioningRules[glis,topos]
