(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopGLIExpand*)


(* ::Text:: *)
(*`FCLoopGLIExpand[exp, topos, {x, x0, n}]` expands `GLI`s defined via the list of*)
(*topologies `topos` in `exp` around `x=x0` to order `n`. Here `x` must be a scalar quantity, e.g.*)
(*a mass or a scalar product.*)


(* ::Text:: *)
(*This routine is particularly useful for doing asymptotic expansions of integrals or amplitudes.*)


(* ::Text:: *)
(*Notice that the series is assumed to be well-defined. The function has no built-in checks against singular behavior.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopGLIDifferentiate](FCLoopGLIDifferentiate.md), [ToGFAD](ToGFAD.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopGLIExpand[x GLI[tad2l,{1,1,1}],
{FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]},{m1,0,2}]


FCLoopGLIExpand[x GLI[tad2l,{1,1,1}],
{FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]},{m1,M,4}]


FCLoopGLIExpand[m2^2 GLI[tad2l,{1,1,1}],
{FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]},{m2,0,6}]


FCLoopGLIExpand[ GLI[prop1l,{1,1}]+SPD[q] GLI[prop1l,{1,0}],
{FCTopology[prop1l, {FAD[{p1, m1}], FAD[{p1+q, m2}]}, {p1}, {q}, {}, {}]},{SPD[q],0,1}]
