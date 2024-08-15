(* ::Package:: *)

(* ::Section:: *)
(*FCLoopReplaceQuadraticEikonalPropagators*)


(* ::Text:: *)
(*`FCLoopReplaceQuadraticEikonalPropagators[topologies]` identifies `SFAD`s and `CFAD`s in `topologies` that represent mixed quadratic-eikonal propagators, e.g. $[p^2 - 2 p \cdot q]$. Using the information on loop momenta provided by the user the routine will try to rewrite those denominators by completing the square, e.g. as in $[(p-q)^2 - q^2]$.*)


(* ::Text:: *)
(*This procedure is useful because one cannot easily determine the momentum flow from looking at quadratic-eikonal propagators as it is possible in the case of purely quadratic ones.*)


(* ::Text:: *)
(*For this to work it is crucial to specify the loop momenta via the `LoopMomenta` option as well as the kinematics (`IntermediateSubstitutions`) and the rules for completing the square (`InitialSubstitutions`) on the purely loop-momentum dependent piece of the propagator (e.g. $p_1^2 - 2 p_1 \cdot p_2 + p_2^2$ goes to $(p_1+p_2)^2$.*)


(* ::Text:: *)
(*Internally this routine uses `ToGFAD` and `FromGFAD`.*)


(* ::Subsection:: *)
(*See also*)


(* ::Text:: *)
(*[Overview](Extra/FeynCalc.md), [FCTopology](FCTopology.md), [GFAD](GFAD.md), [FromGFAD](FromGFAD.md), [ToGFAD](ToGFAD.md).*)


(* ::Subsection:: *)
(*Examples*)


(DataType[#,FCVariable]=True)&/@\!\(TraditionalForm\`{gkin, meta, u0b}\);


topos={FCTopology[preTopoDia1,{SFAD[{{k2,0},{0,1},1}],SFAD[{{k1,0},{0,1},1}],
SFAD[{{k1+k2,0},{0,1},1}],SFAD[{{0,-k1.nb},{0,1},1}],SFAD[{{k2,-(meta*u0b*k2.nb)},{0,1},1}],
SFAD[{{k1+k2,-2*gkin*meta*u0b*(k1+k2).n},{0,1},1}],SFAD[{{k1,-2*gkin*meta*k1.n+meta*u0b*k1.nb},
{2*gkin*meta^2*u0b,1},1}],SFAD[{{k1,-2*gkin*meta*u0b*k1.n+meta*u0b*k1.nb},{2*gkin*meta^2*u0b^2,1},1}]},
{k1,k2},{n,nb},{Hold[SPD][n]->0,Hold[SPD][nb]->0,Hold[SPD][n,nb]->2},{}]}


FCLoopReplaceQuadraticEikonalPropagators[topos,LoopMomenta->{k1,k2},
InitialSubstitutions->{
ExpandScalarProduct[SPD[k1-k2]]->SPD[k1-k2],
ExpandScalarProduct[SPD[k1+k2]]->SPD[k1+k2]},
IntermediateSubstitutions->{SPD[n]->0,SPD[nb]->0,SPD[n,nb]->0}]
