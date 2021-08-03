(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopFindTopologies*)


(* ::Text:: *)
(*`FCLoopFindTopologies[exp, {q1, q2, ...}]` attempts to identify the loop integral topologies present in `exp` by looking at the propagator denominators that depend on the loop momenta `q1, q2, ...` . It returns a list of two entries, where the first one is the original expression with the denominators rewritten as `GLI`s, and the second one is the set of the identified topologies. Each of the identified topologies must contain linearly independent propagators (unless the option `FCLoopBasisOverdeterminedQ` is set to True), but may lack propagators needed to form a complete basis.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[FCTopology](FCTopology), [GLI](GLI).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopFindTopologies[GFAD[{{-SPD[p1,p2]+SPD[p1,Q]*SPD[p2,Q],1},1}]
*HoldForm[cc1]*SFAD[{{p1,0},{0,1},1}]*SFAD[{{p2,0},{0,1},1}]*SFAD[{{
p3,0},{0,1},1}]*SFAD[{{p1-Q,0},{0,1},2}]*SFAD[{{p1+p2-Q,0},{0,1},1}]*
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]+GFAD[{{-SPD[p1,p2]+SPD[p1,Q]*SPD[p2,Q],
1},1}]*HoldForm[cc2]*SFAD[{{p1,0},{0,1},1}]*SFAD[{{p2,0},{0,1},1}]*
SFAD[{{p3,0},{0,1},1}]*SFAD[{{p1-Q,0},{0,1},1}]*SFAD[{{p1+p2-Q,0},{0,
1},1}]*SFAD[{{p1+p3-Q,0},{0,1},1}]*SFAD[{{p1+p2+p3-Q,0},{0,1},1}]+
GFAD[{{-SPD[p1,p2]+SPD[p1,Q]*SPD[p2,Q],1},1}]*HoldForm[cc3]*SFAD[{{p1,
0},{0,1},1}]*SFAD[{{p2,0},{0,1},1}]*SFAD[{{p3,0},{0,1},1}]*SFAD[{{p1-
Q,0},{0,1},1}]*SFAD[{{p2-Q,0},{0,1},1}]*SFAD[{{p2+p3-Q,0},{0,1},1}]*
SFAD[{{p1+p2+p3-Q,0},{0,1},1}]+GFAD[{{-SPD[p1,p2]+SPD[p1,Q]*SPD[p2,Q],
1},1}]*HoldForm[cc4]*SFAD[{{p1,0},{0,1},1}]*SFAD[{{p2,0},{0,1},1}]*
SFAD[{{p3,0},{0,1},1}]*SFAD[{{p2+p3,0},{0,1},1}]*SFAD[{{p1-Q,0},{0,1},
1}]*SFAD[{{p1+p2-Q,0},{0,1},1}]*SFAD[{{p2+p3-Q,0},{0,1},1}]*SFAD[{{p1+
p2+p3-Q,0},{0,1},1}]+cc5*GFAD[{{-SPD[p1,p2]+SPD[p1,Q]*SPD[p2,Q],1},1}]
*SFAD[{{p1,0},{0,1},1}]*SFAD[{{p2,0},{0,1},1}]*SFAD[{{p3,0},{0,1},1}]*
SFAD[{{p2-Q,0},{0,1},1}]*SFAD[{{p1+p3-Q,0},{0,1},1}]*SFAD[{{p2+p3-Q,0}
,{0,1},1}]*SFAD[{{p1+p2+p3-Q,0},{0,1},1}],{p1,p2,p3},FCPrint->
False,FCE->True]
