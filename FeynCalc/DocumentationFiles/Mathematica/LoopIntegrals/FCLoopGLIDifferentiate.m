(* ::Package:: *)

 


(* ::Section:: *)
(*FCLoopGLIDifferentiate*)


(* ::Text:: *)
(*`FCLoopGLIDifferentiate[exp , topos, inv]` calculates the partial derivative of GLIs present in `exp` with respect to the scalar quantity `inv`.*)
(*Here `inv` can be a constant (e.g. mass), a scalar product of some momenta or a 4-vector.*)


(* ::Text:: *)
(*The list topos must contain the topologies describing all of the occurring GLIs.*)


(* ::Text:: *)
(*To calculate multiple derivatives, use the notation `FCLoopGLIDifferentiate[exp , topos, {inv,n}]` for scalars and*)
(*`FCLoopGLIDifferentiate[exp , topos, {vec1, vec2, ...}]` for vectors.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GLI](GLI.md), [FCLoopGLIExpand](FCLoopGLIExpand.md).*)


(* ::Subsection:: *)
(*Examples*)


FCLoopGLIDifferentiate[x GLI[tad2l,{1,1,1}],
{FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]},m1]


FCLoopGLIDifferentiate[x GLI[tad2l,{1,1,1}],
{FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]},{m1,5}]


FCLoopGLIDifferentiate[m2^2 GLI[tad2l,{1,1,1}],
{FCTopology[tad2l, {FAD[{p1, m1}], FAD[{p2, m2}], FAD[{p1 - p2, m3}]}, {p1, p2}, {}, {}, {}]},m2]


FCLoopGLIDifferentiate[ GLI[prop1l,{1,1}]+SPD[q] GLI[prop1l,{1,0}],
{FCTopology[prop1l, {FAD[{p1, m1}], FAD[{p1+q, m2}]}, {p1}, {q}, {}, {}]},SPD[q]]


FCLoopGLIDifferentiate[SPD[p1,p2]GLI[topo1,{1,1,1}],
{FCTopology[topo1,{SFAD[{p1,m1^2}],SFAD[{p2,m2^2}],SFAD[p1-p2]},{p1,p2},{},{},{}]},
FVD[p1,mu],FCE->True]


FCLoopGLIDifferentiate[SPD[p1,p2]GLI[topo1,{1,1,1}],{FCTopology[topo1,
{SFAD[{p1,m1^2}],SFAD[{p2,m2^2}],SFAD[p1-p2]},{p1,p2},{},{},{}]}
,{FVD[p1,mu],FVD[p2,nu]},FCE->True]
